package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.System.CommandFailure;
import runci.Config.*;

class Cpp {
	static public var gotCppDependencies = false;
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
		Sys.putEnv("CPPIA_IS_JIT", "1");
		run(cmd, args.concat([script, "-jit"]));
		Sys.putEnv("CPPIA_IS_JIT", null);
	}

	static final LLDB_ITERATIONS = 30;

	/**
		DEBUG(mac-arm64-cppia-jit): runs the cppia JIT under lldb repeatedly, to catch the
		intermittent `hxSehException` crash on macOS arm64 that happens while the module
		is being JIT-compiled. lldb stops on the EXC_BAD_ACCESS before hxcpp's SIGSEGV
		handler converts it into an `hxSehException`, so we get a native backtrace.
	**/
	static function debugCppiaJitWithLldb(script:String, host:String) {
		final lldbArgs = [
			"--batch",
			// lldb disables ASLR by default, and the crash didn't reproduce in 30 runs that way
			"-O", "settings set target.disable-aslr false",
			"-o", "run",
			// these only run if the process stops because of a crash
			"-k", "thread backtrace all",
			"-k", "register read",
			"-k", "image lookup --address $pc",
			"-k", "disassemble --pc --count 24",
			"-k", "image list",
			"-k", "quit 1",
			"--", host, script, "-jit"
		];
		final cmd = "lldb " + [for (a in lldbArgs) "'" + a.split("'").join("'\\''") + "'"].join(" ") + " 2>&1";
		final exitedRegex = ~/Process \d+ exited with status = (-?\d+)/;
		Sys.putEnv("CPPIA_IS_JIT", "1");
		for (i in 1...LLDB_ITERATIONS + 1) {
			infoMsg('lldb cppia jit run $i/$LLDB_ITERATIONS: $cmd');
			final p = new sys.io.Process("/bin/sh", ["-c", cmd]);
			final output = p.stdout.readAll().toString();
			final exitCode = p.exitCode();
			p.close();
			final crashed = output.indexOf("stop reason =") != -1;
			final exited = exitedRegex.match(output);
			if (crashed || !exited || exitedRegex.matched(1) != "0") {
				Sys.println(output);
				failMsg('lldb cppia jit run $i failed (lldb exit code $exitCode, crashed: $crashed)');
				Sys.putEnv("CPPIA_IS_JIT", null);
				throw new CommandFailure(exitCode == 0 ? 1 : exitCode);
			}
			if (i == 1)
				Sys.println(output);
			successMsg('lldb cppia jit run $i passed');
		}
		Sys.putEnv("CPPIA_IS_JIT", null);
	}

	static public function run(args:Array<String>, testCompiled:Bool, testCppia:Bool) {
		Cpp.testCppia = testCppia;

		getCppDependencies();

		if (testCompiled) {
			runCommand("haxe", ["compile-cpp.hxml"].concat(args));
			runCpp("bin/cpp/TestMain-debug", []);

			changeDirectory(sysDir);
			runCommand("haxe", args.concat(["compile-cpp.hxml"]));
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
			if (systemName == "Mac" && System.arch == Arm64)
				debugCppiaJitWithLldb("bin/unit.cppia", FileSystem.fullPath("bin/cppia/Host-debug"));
			runCppia("bin/unit.cppia", FileSystem.fullPath("bin/cppia/Host-debug"));

			changeDirectory(sysDir);
			runCommand("haxe", args.concat(["compile-cppia.hxml"]));
			runCppia("bin/cppia/Main.cppia", runSysTest);
		}
	}
}
