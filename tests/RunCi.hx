import haxe.Exception;
import runci.TestTarget;
import runci.System;
import runci.System.*;
import runci.Config.*;
import runci.Deployment.*;

using StringTools;

/**
	Will be run by CI services, currently TravisCI and AppVeyor.

	TravisCI:
	Setting file: ".travis.yml".
	Build result: https://travis-ci.org/HaxeFoundation/haxe

	AppVeyor:
	Setting file: "appveyor.yml".
	Build result: https://ci.appveyor.com/project/HaxeFoundation/haxe
*/
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

		for (test in tests) {
			switch (ci) {
				case TravisCI:
					Sys.println('travis_fold:start:test-${test}');
				case _:
					//pass
			}

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
			var success = true;
			try {
				changeDirectory(unitDir);
				haxelibInstallGit("haxe-utest", "utest", "master");

				var args = switch (ci) {
					case null:
						[];
					case TravisCI:
						["-D","travis"];
					case AppVeyor:
						["-D","appveyor"];
					case AzurePipelines:
						["-D","azure"];
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
					case Flash9:
						runci.targets.Flash.run(args);
					case Hl:
						runci.targets.Hl.run(args);
					case t:
						throw new Exception("unknown target: " + t);
				}
			} catch(f:Failure) {
				success = false;
			}

			switch (ci) {
				case TravisCI:
					Sys.println('travis_fold:end:test-${test}');
				case _:
					//pass
			}

			if (success) {
				successMsg('test ${test} succeeded');
			} else {
				failMsg('test ${test} failed');
			}

			echoServer.kill();
			echoServer.close();
		}

		if (success) {
			deploy();
		} else {
			Sys.exit(1);
		}
	}
}
