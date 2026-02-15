package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Cpp {
	static public var gotCppDependencies = false;
	static final miscCppDir = getMiscSubDir('cpp');

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
			final oldDir = Sys.getCwd();
			changeDirectory(getHaxelibPath("hxcpp") + "tools/hxcpp/");
			runCommand("haxe", ["-D", "source-header=''", "compile.hxml"]);
			changeDirectory(oldDir);
		}

		gotCppDependencies = true;
	}

	static public function runCpp(bin:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		bin = FileSystem.fullPath(bin);
		runCommand(bin, args);
	}

	static public function run(args:Array<String>, testCompiled:Bool, testCppia:Bool) {
		getCppDependencies();

		if (testCompiled) {
			runCommand("rm", ["-rf", "cpp"]);
			runCommand("haxe", ["compile-cpp.hxml"].concat(args));
			runCpp("bin/cpp/TestMain-debug", []);
		}

		if (testCppia) {
			runCommand("haxe", ["compile-cppia-host.hxml"].concat(args));
			runCommand("haxe", ["compile-cppia.hxml"].concat(args));
			runCpp("bin/cppia/Host-debug", ["bin/unit.cppia"]);

			if (!(systemName == 'Linux' && System.arch == Arm64)) // FIXME
				runCpp("bin/cppia/Host-debug", ["bin/unit.cppia", "-jit"]);
		}

		Display.maybeRunDisplayTests(Cpp);

		changeDirectory(sysDir);
		runCommand("haxe", ["--each", "compile-cpp.hxml"].concat(args));
		runSysTest(FileSystem.fullPath("bin/cpp/Main-debug"));

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-cpp", "export/cpp"]);
		runCpp("export/cpp/Main");

		changeDirectory(getMiscSubDir("eventLoop"));
		runCommand("haxe", ["build-cpp.hxml"]);
		// TODO: check output like misc tests do
		runCpp("cpp/Main");

		if (Sys.systemName() == "Mac") {
			changeDirectory(getMiscSubDir("cppObjc"));
			runCommand("haxe", ["build.hxml"]);
			runCpp("bin/TestObjc-debug");
		}

		changeDirectory(miscCppDir);
		runCommand("haxe", ["run.hxml"]);
	}
}
