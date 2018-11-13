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
		var RUN_BENCHMARKS = true;

		var args = Sys.args();
		var tests:Array<TestTarget> = switch (args.length==1 ? args[0] : Sys.getEnv("TEST")) {
			case null:
				[Macro];
			case env:
				[for (v in env.split(",")) v.trim().toLowerCase()];
		}

		infoMsg('Going to test: $tests');

		for (test in tests) {
			switch (ci) {
				case TravisCI:
					Sys.println('travis_fold:start:test-${test}');
				case _:
					//pass
			}

			infoMsg('test $test');
			var success = true;
			try {
				changeDirectory(unitDir);


				var args = switch (ci) {
					case TravisCI:
						["-D","travis"];
					case AppVeyor:
						["-D","appveyor"];
					case _:
						[];
				}
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
					case Cs:
						runci.targets.Cs.run(args);
					case Flash9:
						runci.targets.Flash.run(args);
					case As3:
						runci.targets.As3.run(args);
					case Hl:
						runci.targets.Hl.run(args);
					case t:
						throw "unknown target: " + t;
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

			// --- BENCHMARKS
			// this runs after each test target is successful and so properly set up, so that we don't have to re-setup them

			if (success && RUN_BENCHMARKS) {

				switch (ci) {
					case TravisCI:
						Sys.println('travis_fold:start:bench-${test}');
					case _:
						//pass
				}

				infoMsg('bench $test');
				var benchSuccess = true;
				try {
					changeDirectory(benchsDir);


					switch (test) {
						case Macro:
							runci.targets.Macro.runBench(args);
						case Neko:
							runci.targets.Neko.runBench(args);
						case Php:
						//	runci.targets.Php.runBench(args);
						case Python:
						//	runci.targets.Python.runBench(args);
						case Lua:
						//	runci.targets.Lua.runBench(args);
						case Cpp:
						//	runci.targets.Cpp.runBench(args, true, true);
						case Cppia:
						//	runci.targets.Cpp.runBench(args, false, true);
						case Js:
						//	runci.targets.Js.runBench(args);
						case Java:
						//	runci.targets.Java.runBench(args);
						case Cs:
						//	runci.targets.Cs.runBench(args);
						case Flash9:
						//	runci.targets.Flash.runBench(args);
						case As3:
						//	runci.targets.As3.runBench(args);
						case Hl:
						//	runci.targets.Hl.runBench(args);
						case t:
							throw "unknown target: " + t;
					}
				} catch(f:Failure) {
					benchSuccess = false;
				}

				switch (ci) {
					case TravisCI:
						Sys.println('travis_fold:end:bench-${test}');
					case _:
						//pass
				}

				if (benchSuccess) {
					successMsg('bench ${test} succeeded');
				} else {
					failMsg('bench ${test} failed');
				}
			}
			// --- end BENCHMARKS
		}

		if (success) {
			deploy();
		} else {
			Sys.exit(1);
		}
	}
}
