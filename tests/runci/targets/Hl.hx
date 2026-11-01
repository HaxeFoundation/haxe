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
		if (systemName == "Windows") {
			Sys.putEnv("HASHLINK_SRC", hlInstallDir);
			Sys.putEnv("HASHLINK_BIN", hlInstallBinDir);
		}

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
				Linux.requireAptPackages(["libpng-dev", "libjpeg-turbo8-dev", "libturbojpeg", "zlib1g-dev", "libvorbis-dev", "libsqlite3-dev", "gdb"]);
			case "Mac":
			case "Windows":
				//pass
		}

		FileSystem.createDirectory(hlBuild);
		final args = systemName == "Windows" ? ["-DCMAKE_SYSTEM_VERSION=10.0.19041.0"] : ["-GNinja"];
		if (systemName == "Mac") {
			args.push("-DDOWNLOAD_DEPENDENCIES=ON");
		}
		if (systemName == "Linux")
			args.push("-DCMAKE_C_FLAGS=-g -fno-omit-frame-pointer"); // TEMP: crash diagnostics
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

	// TEMP: run the HL/C unit tests repeatedly to catch an intermittent segfault with a backtrace
	static function runHlcCrashDiagnostics(exe:String) {
		final script = '
			crashes=0
			for i in $$(seq 1 60); do
				./$exe > /tmp/hlc-plain.log 2>&1
				r=$$?
				if [ $$r -ne 0 ]; then
					crashes=$$((crashes+1))
					echo "plain run $$i exited with $$r"
					tail -5 /tmp/hlc-plain.log
				fi
			done
			echo "plain runs: $$crashes crashes out of 60"
			for i in $$(seq 1 60); do
				gdb -q -batch -ex "set pagination off" -ex run -ex "bt 40" -ex "info registers" -ex "x/8i $$pc-16" -ex "thread apply all bt 25" --args ./$exe > /tmp/hlc-gdb.log 2>&1
				if grep -q "received signal" /tmp/hlc-gdb.log; then
					crashes=$$((crashes+1))
					echo "===== gdb run $$i crashed ====="
					grep -v -e "^Running " -e "^    test" /tmp/hlc-gdb.log | tail -300
					grep -B3 "received signal" /tmp/hlc-gdb.log
				fi
			done
			echo "total crashes: $$crashes"
			[ $$crashes -eq 0 ]
		';
		runCommand("bash", ["-c", script]);
	}

	static function buildAndRunHlc(dir:String, filename:String, ?run) {
		if (run == null) run = runCommand;

		final compiler = if (systemName == "Mac") "clang" else "gcc";
		final extraCompilerFlags = switch (systemName) {
			case "Windows": ["-ldbghelp", "-municode"];
			case "Mac": ["-rpath", hlInstallLibDir];
			case _: ['-Wl,-rpath,$hlInstallLibDir', "-g"];
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
				'-property:Platform=$msbuildPlatform'
			]);
			run('$dir${msbuildPlatform == 'x64' ? '/x64' : ''}/Release/$filename.exe', []);
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
				buildAndRunHlc("bin/hlc", "unit", (cmd, ?args) -> {
					if (systemName == "Linux")
						runHlcCrashDiagnostics(cmd);
					runCommand(cmd, args);
				});
			}
		}

		changeDirectory(threadsDir);
		buildAndRun("build.hxml", "export/threads");

		changeDirectory(sysDir);
		if (Hl.withJitTests) {
			runCommand("haxe", haxeArgs.concat(["compile-hl.hxml"]));
			runSysTest(hlBinary, ["bin/hl/sys.hl"]);
		}
		if (Hl.withHlcTests) {
			runCommand("haxe", haxeArgs.concat(["compile-hlc.hxml"]));
			function dontRun(cmd,?args) {}
			buildAndRunHlc("bin/hlc/testArguments", "TestArguments", dontRun);
			buildAndRunHlc("bin/hlc/exitCode", "ExitCode", dontRun);
			buildAndRunHlc("bin/hlc/utilityProcess", "UtilityProcess", dontRun);
			buildAndRunHlc("bin/hlc/sys", "sys", (cmd, ?args) -> runSysTest(FileSystem.fullPath(cmd), args));
		}

		changeDirectory(getMiscSubDir("cross", "eventLoop"));
		buildAndRun("build-hl.hxml", "bin/eventLoop");

		// these are generic tests for genhl which shouldn't actually execute the .hl using hl jit,
		// so they are not skipped with --skip-hl-jit
		changeDirectory(hlcodeDir);
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "hl"]);

		if (Hl.withHlcTests) {
			final hlcTemplateDefine = systemName == "Windows" ? "hlgen.makefile=vs2026" : "hlgen.makefile=make";
			runCommand("haxe", ["run-base.hxml", "--run", "Main", "hlc", "-D", hlcTemplateDefine].concat(haxeArgs));
		}
	}
}
