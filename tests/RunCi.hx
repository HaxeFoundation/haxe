import haxe.Exception;
import runci.TestTarget;
import runci.System;
import runci.System.*;
import runci.Config.*;
import runci.Deployment.*;

using StringTools;

class RunCi {
	static function main():Void {
		Sys.putEnv("OCAMLRUNPARAM", "b");

		var args = Sys.args();
		var tests:Array<TestTarget> = switch (args.length==1 ? args[0] : Sys.getEnv("TEST")) {
			case null:
				[Macro];
			case env:
				[for (v in env.split(",")) v.trim().toLowerCase()];
		}

		infoMsg('Going to test: $tests');

		if (isCi()) {
			changeDirectory('echoServer');
			runCommand('haxe', ['build.hxml']);
			changeDirectory(cwd);
		}

		final downloadPath = getDownloadPath();
		if (!sys.FileSystem.exists(downloadPath))
			sys.FileSystem.createDirectory(downloadPath);

		for (test in tests) {
			switch (systemName) {
				case "Windows":
					// change codepage to UTF-8
					runCommand("chcp", ["65001"]);
				case _:
					//pass
			}

			//run neko-based http echo server
			var echoServer = new sys.io.Process('nekotools', ['server', '-d', 'echoServer/www/', '-p', '20200']);

			infoMsg('test $test');
			try {
				changeDirectory(unitDir);
				haxelibInstallGit("haxe-utest", "utest", "master", "--always");

				var args = switch (ci) {
					case null:
						[];
					case GithubActions:
						["-D","github"];
				}
				args = args.concat(["-D", systemName]);
				switch (test) {
					case Macro:
						runci.targets.Macro.run(args);
					case Neko:
						runci.targets.Neko.run(args);
					case Php:
						runci.targets.Php.run(args);
					case Python:
						runci.targets.Python.run(args);
					case Lua:
						runci.targets.Lua.run(args);
					case Cpp:
						runci.targets.Cpp.run(args, true, true);
					case Cppia:
						runci.targets.Cpp.run(args, false, true);
					case Js:
						runci.targets.Js.run(args);
					case Java:
						runci.targets.Java.run(args);
					case Jvm:
						runci.targets.Jvm.run(args);
					case Cs:
						runci.targets.Cs.run(args);
					case Flash:
						runci.targets.Flash.run(args);
					case Hl:
						runci.targets.Hl.run(args);
					case t:
						throw new Exception("unknown target: " + t);
				}
			} catch(f:CommandFailure) {
				failMsg('test ${test} failed');
				Sys.exit(f.exitCode);
			}

			successMsg('test ${test} succeeded');

			echoServer.kill();
			echoServer.close();
		}

		deploy();
	}
}
