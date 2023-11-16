package runci.targets;

import haxe.io.Path;
import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

using StringTools;

class Hl {
	static final hlSrc = Path.join([getDownloadPath(), "hashlink"]);

	static final hlBuild = Path.join([getInstallPath(), "hashlink_build"]);

	static final hlBuildBinDir = Path.join([getInstallPath(), "hashlink_build", "bin"]);

	static final hlBinary =
		if (isCi() || !commandSucceed("hl", ["--version"])){
			Path.join([hlBuildBinDir, "hl"]) + ((systemName == "Windows") ? ".exe" : "");
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
			"-B" + hlBuild,
			"-H" + hlSrc
		]));
		runCommand("cmake", [
			"--build", hlBuild
		]);

		runCommand(hlBinary, ["--version"]);
		addToPATH(hlBuildBinDir);
		addToLIBPATH(hlBuildBinDir);

		haxelibDev("hashlink", '$hlSrc/other/haxelib/');
	}

	static public function run(args:Array<String>) {
		getHlDependencies();

		runCommand("haxe", ["compile-hl.hxml"].concat(args));
		runCommand(hlBinary, ["bin/unit.hl"]);

		runCommand("haxe", ["compile-hlc.hxml"].concat(args));
		switch (systemName) {
			case "Linux" if (isCi()):
				runCommand("gcc", [
					"-o", "bin/hlc/main",
					"bin/hlc/main.c",
					"-Ibin/hlc/",
					'-I$hlSrc/src',
					'$hlBuildBinDir/fmt.hdll',
					"-lm",
					'-L$hlBuildBinDir', "-lhl"
				]);
				runCommand("bin/hlc/main", []);
			case _:
				// TODO hl/c for mac/windows
		}

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-hl", "export/threads.hl"]);
		runCommand(hlBinary, ["export/threads.hl"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-hl.hxml"].concat(args));
		runSysTest(hlBinary, ["bin/hl/sys.hl"]);

		changeDirectory(getMiscSubDir("eventLoop"));
		runCommand("haxe", ["build-hl.hxml"]);
		// TODO: check output like misc tests do
		runCommand(hlBinary, ["eventLoop.hl"]);

		changeDirectory(getMiscSubDir("hl/reserved-keywords"));
		runCommand("haxe", ["compile.hxml"]);
		switch (systemName) {
			case "Linux" if (isCi()):
				runCommand("gcc", ["-o", "bin/test", "bin/test.c", "-Ibin/", '-I$hlSrc/src', '-L$hlBuildBinDir', "-lhl"]);
				runCommand("bin/test", []);
			case _:
				// TODO hl/c for mac/windows
		}

		changeDirectory(miscHlDir);
		runCommand("haxe", ["run.hxml"]);
	}
}
