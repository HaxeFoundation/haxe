package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

using StringTools;
using haxe.io.Path;

class Hl {
	static final hlSrc = Path.join([getDownloadPath(), "hashlink"]);

	static final hlBuild = Path.join([getDownloadPath(), "hashlink_build"]);

	static final hlBinary = if (!commandSucceed("hl", ["--version"])) {
			Path.join([getInstallPath(), "hashlink", systemName == 'Windows' ? '' : 'bin', "hl" + ((systemName == "Windows") ? ".exe" : "")]);
		} else {
			commandResult(if (systemName == "Windows") "where" else "which", ["hl"]).stdout.trim();
		};

	static final hlInstallBinDir = hlBinary.directory();
	static final hlInstallDir = if (systemName == "Windows") hlInstallBinDir else hlInstallBinDir.directory();
	static final hlInstallLibDir = if (systemName == "Windows") hlInstallDir else Path.join([hlInstallDir, "lib"]);

	static var withJitTests = true;
	static var withHlcTests = true;

	static var msbuildPlatform:String = 'x64';

	static public function getHlDependencies() {
		Sys.putEnv("HASHLINK", hlInstallDir);

		if (FileSystem.exists(hlBinary)) {
			infoMsg('hl has already been installed at $hlBinary.');
			return;
		}
		if (!FileSystem.exists(hlSrc))
			runCommand("git", ["clone", "--depth=1", "https://github.com/HaxeFoundation/hashlink.git", hlSrc]);
		else
			infoMsg("Reusing hashlink repository");

		switch (systemName) {
			case "Linux":
				Linux.requireAptPackages(["libpng-dev", "libjpeg-turbo8-dev", "libturbojpeg", "zlib1g-dev", "libvorbis-dev", "libsqlite3-dev"]);
			case "Mac":
			case "Windows":
				//pass
		}

		FileSystem.createDirectory(hlBuild);
		final args = systemName == "Windows" ? ["-DCMAKE_SYSTEM_VERSION=10.0.19041.0"] : ["-GNinja"];
		if (systemName == "Mac") {
			args.push("-DDOWNLOAD_DEPENDENCIES=ON");
		}
		runCommand("cmake", args.concat([
			"-DBUILD_TESTING=OFF",
			"-DWITH_DIRECTX=OFF",
			"-DWITH_FMT=ON",
			"-DWITH_OPENAL=OFF",
			"-DWITH_SDL=OFF",
			"-DWITH_SQLITE=ON",
			"-DWITH_SSL=ON",
			"-DWITH_UI=OFF",
			"-DWITH_UV=OFF",
			"-DWITH_VIDEO=OFF",
			"-DCMAKE_INSTALL_PREFIX=" + hlInstallDir,
			"-B" + hlBuild,
			"-H" + hlSrc
		]));
		runCommand("cmake", [
			"--build", hlBuild
		]);
		runCommand("cmake", ["--build", hlBuild, "--target", "install"]);

		addToPATH(hlInstallBinDir);
		if (withJitTests) {
			runCommand(hlBinary, ["--version"]);
		}

		haxelibDev("hashlink", '$hlSrc/other/haxelib/');
	}

	static function buildAndRunHlc(dir:String, filename:String, ?run) {
		if (run == null) run = runCommand;

		runCommand("haxelib", ["run", "hashlink", "build", '$dir/$filename.c', "-D", "hlgen.makefile=make"]);
		run('$dir/$filename', []);

		// Run with MSBuild
		if (systemName == "Windows") {
			runCommand("haxelib", ["run", "hashlink", "build", '$dir/$filename.c', "-D", "hlgen.makefile=vs2022"]);
			run('$dir${msbuildPlatform == 'x64' ? '/x64' : ''}/Debug/$filename.exe', []);
		}
	}

	static function buildAndRun(hxml:String, target:String, ?args:Array<String>) {
		if (args == null) args = [];

		if (withJitTests) {
			runCommand("haxe", [hxml, "-hl", '$target/hl-jit.hl'].concat(args));
			runCommand(hlBinary, ['$target/hl-jit.hl']);
		}

		if (withHlcTests) {
			runCommand("haxe", [hxml, "-hl", '$target/hlc.c'].concat(args).concat(["--debug"]));
			buildAndRunHlc(target, "hlc");
		}
	}

	static public function run(testArgs:Array<String>, haxeArgs:Array<String>) {
		final msbuildPlatformArgIndex = testArgs.lastIndexOf("--msbuild-platform");
		if (msbuildPlatformArgIndex != -1) {
			msbuildPlatform = testArgs[msbuildPlatformArgIndex + 1] ?? throw "--msbuild-platform needs an argument";
			testArgs.splice(msbuildPlatformArgIndex, 2);
		}
		withJitTests = !testArgs.remove("--skip-hl-jit");
		withHlcTests = !testArgs.remove("--skip-hlc");

		haxeArgs = haxeArgs.concat(testArgs);

		getHlDependencies();

		for (extraArgs in [[], ["--undefine", "analyzer-optimize"]]) {
			if (Hl.withJitTests) {
				runCommand("haxe", ["compile-hl.hxml"].concat(extraArgs).concat(haxeArgs));
				runCommand(hlBinary, ['bin/unit.hl']);
			}
			if (Hl.withHlcTests) {
				runCommand("haxe", ["compile-hlc.hxml"].concat(extraArgs).concat(haxeArgs));
				buildAndRunHlc("bin/hlc", "main", runCommand);
			}
		}

		changeDirectory(threadsDir);
		buildAndRun("build.hxml", "export/threads");

		changeDirectory(sysDir);
		if (Hl.withJitTests) {
			runCommand("haxe", ["compile-hl.hxml"].concat(haxeArgs));
			runSysTest(hlBinary, ["bin/hl/sys.hl"]);
		}
		if (Hl.withHlcTests) {
			runCommand("haxe", ["compile-hlc.hxml"].concat(haxeArgs));
			function dontRun(cmd,?args) {}
			buildAndRunHlc("bin/hlc/testArguments", "TestArguments", dontRun);
			buildAndRunHlc("bin/hlc/exitCode", "ExitCode", dontRun);
			buildAndRunHlc("bin/hlc/utilityProcess", "UtilityProcess", dontRun);
			buildAndRunHlc("bin/hlc/sys", "main", (cmd, ?args) -> runSysTest(FileSystem.fullPath(cmd), args));
		}

		changeDirectory(getMiscSubDir("cross", "eventLoop"));
		buildAndRun("build-hl.hxml", "bin/eventLoop", haxeArgs);

		// these are generic tests for genhl which shouldn't actually execute the .hl using hl jit,
		// so they are not skipped with --skip-hl-jit
		changeDirectory(hlcodeDir);
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "hl"]);

		if (Hl.withHlcTests) {
			runCommand("haxe", ["run-base.hxml", "--run", "Main", "hlc", "-D", "hlgen.makefile"].concat(haxeArgs));
		}
	}
}
