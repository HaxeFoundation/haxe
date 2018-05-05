package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Cpp {
	static public var gotCppDependencies = false;

	static public function getCppDependencies() {
		if (gotCppDependencies) return;

		//hxcpp dependencies
		switch (systemName) {
			case "Linux":
				Linux.requireAptPackages(["gcc-multilib", "g++-multilib"]);
			case "Mac":
				//pass
		}


		//install and build hxcpp
		try {
			var path = getHaxelibPath("hxcpp");
			infoMsg('hxcpp has already been installed in $path.');
		} catch(e:Dynamic) {
			haxelibInstallGit("HaxeFoundation", "hxcpp", true);
			var oldDir = Sys.getCwd();
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

		switch (ci) {
			case AppVeyor:
				if (testCompiled) {
					runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M32"].concat(args));
					runCpp("bin/cpp/TestMain-debug", []);
				}
			case _:
				if (testCompiled) {
					runCommand("rm", ["-rf", "cpp"]);
					runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M64"].concat(args));
					runCpp("bin/cpp/TestMain-debug", []);
				}

				if (testCppia) {
					runCommand("haxe", ["compile-cppia-host.hxml"]);
					runCommand("haxe", ["compile-cppia.hxml"]);
					runCpp("bin/cppia/Host-debug", ["bin/unit.cppia"]);
					runCpp("bin/cppia/Host-debug", ["bin/unit.cppia", "-jit"]);
					runCommand("haxe", ["compile-cppia.hxml", "-D", "nocppiaast"]);
					runCpp("bin/cppia/Host-debug", ["bin/unit.cppia"]);
					runCpp("bin/cppia/Host-debug", ["bin/unit.cppia", "-jit"]);
				}
		}

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-cpp.hxml"]);
		runCpp("bin/cpp/Main-debug", []);

		// if (Sys.systemName() == "Mac")
		// {
		// 	changeDirectory(miscDir + "cppObjc");
		// 	runCommand("haxe", ["build.hxml"]);
		// 	runCpp("bin/TestObjc-debug");
		// }
	}
}
