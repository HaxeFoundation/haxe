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
		if (isCi() || !commandSucceed("hl", ["--version"])){
			Path.join([hlInstallBinDir, "hl"]) + ((systemName == "Windows") ? ".exe" : "");
		} else {
			commandResult(if(systemName == "Windows") "where" else "which", ["hl"]).stdout.trim();
		};

	static final miscHlDir = getMiscSubDir('hl');

	static public function getHlDependencies() {
		if (!isCi() && FileSystem.exists(hlBinary)) {
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
				runNetworkCommand("brew", ["update", '--preinstall']);
				runNetworkCommand("brew", ["bundle", '--file=${hlSrc}/Brewfile']);
			case "Windows":
				//pass
		}

		FileSystem.createDirectory(hlBuild);
		final generator = systemName == "Windows" ? ["-DCMAKE_SYSTEM_VERSION=10.0.19041.0"] : ["-GNinja"];
		runCommand("cmake", generator.concat([
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
		runCommand(hlBinary, ["--version"]);

		haxelibDev("hashlink", '$hlSrc/other/haxelib/');
	}

	static function buildAndRunHlc(dir:String, filename:String, ?run) {
		if (run == null) run = runCommand;

		if (!isCi())
			return;

		final compiler = if (systemName == "Mac") "clang" else "gcc";
		final extraCompilerFlags = if (systemName == "Windows") ["-ldbghelp", "-municode"] else [];

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
	}

	static function buildAndRun(hxml:String, target:String, ?args:Array<String>) {
		if (args == null) args = [];

		runCommand("haxe", [hxml, "-hl", '$target/hl-jit.hl'].concat(args));
		runCommand(hlBinary, ['$target/hl-jit.hl']);

		runCommand("haxe", [hxml, "-hl", '$target/hlc.c'].concat(args));
		buildAndRunHlc(target, "hlc");
	}

	static public function run(args:Array<String>) {
		getHlDependencies();

		runCommand("haxe", ["compile-hl.hxml"].concat(args));
		runCommand(hlBinary, ['bin/unit.hl']);
		runCommand("haxe", ["compile-hlc.hxml"].concat(args));
		buildAndRunHlc("bin/hlc", "unit", runCommand);

		changeDirectory(threadsDir);
		buildAndRun("build.hxml", "export/threads");

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-hl.hxml"].concat(args));
		runSysTest(hlBinary, ["bin/hl/sys.hl"]);
		runCommand("haxe", ["compile-hlc.hxml"].concat(args));
		function dontRun(cmd,?args) {}
		buildAndRunHlc("bin/hlc/testArguments", "TestArguments", dontRun);
		buildAndRunHlc("bin/hlc/exitCode", "ExitCode", dontRun);
		buildAndRunHlc("bin/hlc/utilityProcess", "UtilityProcess", dontRun);
		buildAndRunHlc("bin/hlc/sys", "sys", (cmd, ?args) -> runSysTest(FileSystem.fullPath(cmd), args));

		changeDirectory(getMiscSubDir("eventLoop"));
		buildAndRun("build-hl.hxml", "bin/eventLoop");

		changeDirectory(getMiscSubDir("hl/reservedKeywords"));
		buildAndRun("compile.hxml", "bin/reservedKeywords");

		changeDirectory(miscHlDir);
		runCommand("haxe", ["run.hxml"]);
	}
}
