package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Cpp {
	static public var gotCppDependencies = false;
	static final isLinuxArm64 = systemName == 'Linux' && System.arch == Arm64;
	static var testCppia = false;

	static public function getCppDependencies() {
		if (gotCppDependencies) return;

		//hxcpp dependencies
		switch (systemName) {
			case "Linux" if (System.arch == Amd64):
				Linux.requireAptPackages(["gcc-multilib", "g++-multilib"]);
			case "Mac":
				//pass
		}


		//install and build hxcpp
		try {
			final path = getHaxelibPath("hxcpp");
			infoMsg('hxcpp has already been installed in $path.');
		} catch(e:Dynamic) {
			haxelibInstallGit("HaxeFoundation", "hxcpp", true);
			runCommand("haxe", [
				"--cwd", getHaxelibPath("hxcpp") + "tools/hxcpp/",
				"-D", "source-header=''",
				"compile.hxml"
			]);
			if (testCppia) {
				final hxmlSuffix = if (System.arch == Arm64) "-arm64" else "";
				runCommand("haxe", [
					"--cwd", getHaxelibPath("hxcpp") + "project/",
					"-D", "source-header=''",
					'compile-cppia${hxmlSuffix}.hxml'
				]);
			}
		}

		gotCppDependencies = true;
	}

	static public function runCpp(bin:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		bin = FileSystem.fullPath(bin);
		runCommand(bin, args);
	}

	static public function runCppia(script:String, ?host:String, ?run:(String, Array<String>) -> Void):Void {
		if (run == null) run = runCommand;
		var cmd:String;
		var args:Array<String>;
		if (host == null) {
			cmd = "haxelib";
			args = ["run", "hxcpp"];
		} else {
			cmd = host;
			args = [];
		}
		run(cmd, args.concat([script]));
		if (!isLinuxArm64) { // FIXME
			Sys.putEnv("CPPIA_IS_JIT", "1");
			run(cmd, args.concat([script, "-jit"]));
			Sys.putEnv("CPPIA_IS_JIT", null);
		}
	}

	static public function run(args:Array<String>, testCompiled:Bool, testCppia:Bool) {
		Cpp.testCppia = testCppia;

		getCppDependencies();

		if (testCompiled) {
			runCommand("haxe", ["compile-cpp.hxml"].concat(args));
			runCpp("bin/cpp/TestMain-debug", []);

			changeDirectory(sysDir);
			runCommand("haxe", ["--each", "compile-cpp.hxml"].concat(args));
			runSysTest(FileSystem.fullPath("bin/cpp/Main-debug"));

			changeDirectory(threadsDir);
			runCommand("haxe", ["build.hxml", "-cpp", "export/cpp"]);
			runCpp("export/cpp/Main");

			changeDirectory(getMiscSubDir("cross", "eventLoop"));
			runCommand("haxe", ["build-cpp.hxml"]);
			// TODO: check output like misc tests do
			runCpp("cpp/Main");

			if (Sys.systemName() == "Mac") {
				changeDirectory(getMiscSubDir("cpp", "cppObjc"));
				runCommand("haxe", ["build.hxml"]);
				runCpp("bin/TestObjc-debug");
			}

			changeDirectory(getMiscSubDir(""));
			runCommand("haxe", ["run-base.hxml", "--run", "Main", "cpp"]);
		}

		if (testCppia) {
			changeDirectory(unitDir);

			runCommand("haxe", ["compile-cppia-host.hxml"].concat(args));
			runCommand("haxe", ["compile-cppia.hxml"].concat(args));
			runCppia("bin/unit.cppia", FileSystem.fullPath("bin/cppia/Host-debug"));

			changeDirectory(sysDir);
			runCommand("haxe", ["compile-cppia.hxml"].concat(args));
			runCppia("bin/cppia/Main.cppia", runSysTest);
		}
	}
}
