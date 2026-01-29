package runci.targets;

import haxe.io.Path;
import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

using StringTools;

class Hl {
	static final hlSrc = Path.join([getDownloadPath(), "hashlink"]);

	static final hlBuild = Path.join([getDownloadPath(), "hashlink_build"]);

	static final hlInstallDir = Path.join([getInstallPath(), "hashlink"]);
	static final hlInstallBinDir = if (systemName == "Windows") hlInstallDir else Path.join([hlInstallDir, "bin"]);
	static final hlInstallLibDir = if (systemName == "Windows") hlInstallDir else Path.join([hlInstallDir, "lib"]);

	static final hlBinary =
		if (!commandSucceed("hl", ["--version"])){
			Path.join([hlInstallBinDir, "hl"]) + ((systemName == "Windows") ? ".exe" : "");
		} else {
			commandResult(if(systemName == "Windows") "where" else "which", ["hl"]).stdout.trim();
		};

	static final miscHlDir = getMiscSubDir('hl');
	static final miscHlcDir = getMiscSubDir('hlc');

	static var withJitTests = true;
	static var withHlcTests = true;

	static public function getHlDependencies() {
		if (FileSystem.exists(hlBinary)) {
			infoMsg('hl has already been installed at $hlBinary.');
			return;
		}
		if (!FileSystem.exists(hlSrc))
			runCommand("git", ["clone", "https://github.com/HaxeFoundation/hashlink.git", hlSrc]);
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
		addToLIBPATH(hlInstallLibDir);
		if (withJitTests) {
			runCommand(hlBinary, ["--version"]);
		}

		haxelibDev("hashlink", '$hlSrc/other/haxelib/');

		Sys.putEnv("HASHLINK", hlInstallDir);
		if (systemName == "Windows") {
			Sys.putEnv("HASHLINK_SRC", hlSrc);
			Sys.putEnv("HASHLINK_BIN", hlInstallBinDir);
		}
	}

	static function buildAndRunHlc(dir:String, filename:String, ?run) {
		if (run == null) run = runCommand;

		final compiler = if (systemName == "Mac") "clang" else "gcc";
		final extraCompilerFlags = switch (systemName) {
			case "Windows": ["-ldbghelp", "-municode"];
			case _: [];
		};

		runCommand(compiler, [
			"-o", '$dir/$filename.exe',
			'$dir/$filename.c',
			'-I$dir',
			'-I$hlInstallDir/include',
			'-L$hlInstallLibDir',
			'$hlInstallLibDir/fmt.hdll',
			'$hlInstallLibDir/ssl.hdll',
			'$hlInstallLibDir/sqlite.hdll',
			"-lm",
			"-lhl"
		].concat(extraCompilerFlags));

		run('$dir/$filename.exe', []);

		// Run with MSBuild
		if (systemName == "Windows") {
			runCommand("MSBuild.exe", [
				'$dir/$filename.sln',
				'-nologo', '-verbosity:minimal',
				'-t:$filename',
				'-property:Configuration=Release',
				'-property:Platform=x64'
			]);
			run('$dir/x64/Release/$filename.exe', []);
		}
	}

	static function buildAndRun(hxml:String, target:String, ?args:Array<String>) {
		if (args == null) args = [];

		if (withJitTests) {
			runCommand("haxe", [hxml, "-hl", '$target/hl-jit.hl'].concat(args));
			runCommand(hlBinary, ['$target/hl-jit.hl']);
		}

		if (withHlcTests) {
			runCommand("haxe", [hxml, "-hl", '$target/hlc.c', "-D", "hlgen.makefile=ci"].concat(args));
			buildAndRunHlc(target, "hlc");
		}
	}

	static public function run(args:Array<String>, withJitTests:Bool, withHlcTests:Bool) {
		Hl.withJitTests = withJitTests;
		Hl.withHlcTests = withHlcTests;

		getHlDependencies();

		for (extraArgs in [[], ["--undefine", "analyzer-optimize"]]) {
			if (Hl.withJitTests) {
				runCommand("haxe", ["compile-hl.hxml"].concat(extraArgs).concat(args));
				runCommand(hlBinary, ['bin/unit.hl']);
			}
			if (Hl.withHlcTests) {
				runCommand("haxe", ["compile-hlc.hxml"].concat(extraArgs).concat(args));
				buildAndRunHlc("bin/hlc", "unit", runCommand);
			}
		}

		changeDirectory(threadsDir);
		buildAndRun("build.hxml", "export/threads");

		Display.maybeRunDisplayTests(Hl);

		changeDirectory(sysDir);
		if (Hl.withJitTests) {
			runCommand("haxe", ["compile-hl.hxml"].concat(args));
			runSysTest(hlBinary, ["bin/hl/sys.hl"]);
		}
		if (Hl.withHlcTests) {
			runCommand("haxe", ["compile-hlc.hxml"].concat(args));
			function dontRun(cmd,?args) {}
			buildAndRunHlc("bin/hlc/testArguments", "TestArguments", dontRun);
			buildAndRunHlc("bin/hlc/exitCode", "ExitCode", dontRun);
			buildAndRunHlc("bin/hlc/utilityProcess", "UtilityProcess", dontRun);
			buildAndRunHlc("bin/hlc/sys", "sys", (cmd, ?args) -> runSysTest(FileSystem.fullPath(cmd), args));
		}

		changeDirectory(getMiscSubDir("eventLoop"));
		buildAndRun("build-hl.hxml", "bin/eventLoop");

		// these are generic tests for genhl which shouldn't actually execute the .hl using hl jit,
		// so they are not skipped with --skip-hl-jit
		changeDirectory(miscHlDir);
		runCommand("haxe", ["run.hxml"]);

		if (Hl.withHlcTests) {
			final hlcTemplateDefine = systemName == "Windows" ? "hlgen.makefile=vs2022" : "hlgen.makefile=make";
			changeDirectory(getMiscSubDir("hlc/reservedKeywords"));
			runCommand("haxe", ["compile.hxml", "-D", hlcTemplateDefine]);
			buildAndRunHlc("bin", "reservedKeywords");

			changeDirectory(miscHlcDir);
			runCommand("haxe", ["run.hxml", "-D", hlcTemplateDefine]);
		}
	}
}
