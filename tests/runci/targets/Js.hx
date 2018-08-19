package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.Path;
import sys.io.Process;

using StringTools;

class Js {
	static public function getJSDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					Linux.requireAptPackages(["nodejs"]);
				}
			case "Mac":
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getJSDependencies();

		var jsOutputs = [
			for (es3 in       [[], ["-D", "js-es=3"]])
			for (unflatten in [[], ["-D", "js-unflatten"]])
			for (classic in   [[], ["-D", "js-classic"]])
			for (enums_as_objects in [[], ["-D", "js-enums-as-arrays"]])
			{
				var extras = args.concat(es3).concat(unflatten).concat(classic).concat(enums_as_objects);

				runCommand("haxe", ["compile-js.hxml"].concat(extras));

				var output = if (extras.length > 0) {
					"bin/js/" + extras.join("") + "/unit.js";
				} else {
					"bin/js/default/unit.js";
				}
				var outputDir = Path.directory(output);
				if (!FileSystem.exists(outputDir)) {
					FileSystem.createDirectory(outputDir);
				}
				FileSystem.rename("bin/unit.js", output);
				FileSystem.rename("bin/unit.js.map", output + ".map");
				runCommand("node", ["-e", "require('./" + output + "').unit.TestMain.nodejsMain();"]);
				output;
			}
		];

		haxelibInstall("hxnodejs");
		var env = Sys.environment();
		if (
			env.exists("SAUCE") &&
			env.exists("SAUCE_USERNAME") &&
			env.exists("SAUCE_ACCESS_KEY")
		) {
			// sauce-connect should have been started

			// var scVersion = "sc-4.3-linux";
			// runCommand("wget", ['https://saucelabs.com/downloads/${scVersion}.tar.gz'], true);
			// runCommand("tar", ["-xf", '${scVersion}.tar.gz']);

			// //start sauce-connect
			// var scReadyFile = "sauce-connect-ready-" + Std.random(100);
			// var sc = new Process('${scVersion}/bin/sc', [
			// 	"-i", Sys.getEnv("TRAVIS_JOB_NUMBER"),
			// 	"-f", scReadyFile
			// ]);
			// while(!FileSystem.exists(scReadyFile)) {
			// 	Sys.sleep(0.5);
			// }

			runCommand("npm", ["install", "wd", "q"], true);
			runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
			var server = new Process("nekotools", ["server"]);
			runCommand("node", ["bin/RunSauceLabs.js"].concat([for (js in jsOutputs) "unit-js.html?js=" + js.urlEncode()]));

			server.close();
			// sc.close();
		}

		infoMsg("Test optimization:");
		changeDirectory(optDir);
		runCommand("haxe", ["run.hxml"]);
		haxelibInstall("utest");
		// changeDirectory(serverDir);
		// runCommand("haxe", ["build.hxml"]);
		// runCommand("node", ["test.js"]);
	}
}