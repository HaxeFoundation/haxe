package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.Path;
import sys.io.Process;

using StringTools;

class Js {
	static final miscJsDir = miscDir + 'js/';

	static public function getJSDependencies() {
		switch [ci, systemName] {
			case [_, "Linux"]:
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					Linux.requireAptPackages(["nodejs"]);
				}
			case _:
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getJSDependencies();

		var jsOutputs = [
			for (es_ver in    [[], ["-D", "js-es=3"], ["-D", "js-es=6"]])
			for (unflatten in [[], ["-D", "js-unflatten"]])
			for (classic in   [[], ["-D", "js-classic"]])
			for (enums_as_objects in [[], ["-D", "js-enums-as-arrays"]])
			{
				var extras = args.concat(es_ver).concat(unflatten).concat(classic).concat(enums_as_objects);

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
				runCommand("node", ["-e", "require('./" + output + "').unit.TestMain.main();"]);
				output;
			}
		];

		infoMsg("Test ES6:");
		changeDirectory(miscDir + "es6");
		runCommand("haxe", ["run.hxml"]);

		haxelibInstallGit("HaxeFoundation", "hxnodejs");
		var env = Sys.environment();
		if (
			env.exists("SAUCE") &&
			env.exists("SAUCE_USERNAME") &&
			env.exists("SAUCE_ACCESS_KEY")
		) {
			var sc = switch (ci) {
				// TODO: figure out SauceConnect for GitHub Actions
				// case AzurePipelines:
				// 	var scVersion = "sc-4.5.3-linux";
				// 	runCommand("wget", ["-q", 'https://saucelabs.com/downloads/${scVersion}.tar.gz'], true);
				// 	runCommand("tar", ["-xf", '${scVersion}.tar.gz']);

				// 	//start sauce-connect
				// 	var scReadyFile = "sauce-connect-ready-" + Std.random(100);
				// 	var p = new Process('${scVersion}/bin/sc', [
				// 		"-i", Sys.getEnv("SAUCE_TUNNEL_ID"),
				// 		"-f", scReadyFile
				// 	]);
				// 	while(!FileSystem.exists(scReadyFile)) {
				// 		Sys.sleep(0.5);
				// 	}
				// 	p;
				case _:
					// sauce-connect should have been started
					null;
			}

			changeDirectory(unitDir);
			runCommand("npm", ["install", "wd", "q"], true);
			runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
			var server = new Process("nekotools", ["server"]);
			runCommand("node", ["bin/RunSauceLabs.js"].concat([for (js in jsOutputs) "unit-js.html?js=" + js.urlEncode()]));

			server.close();

			if (sc != null)
				sc.close();
		}

		infoMsg("Test optimization:");
		changeDirectory(optDir);
		runCommand("haxe", ["run.hxml"]);

		runci.targets.Java.getJavaDependencies(); // this is awkward
		haxelibInstallGit("Simn", "haxeserver");
		changeDirectory(serverDir);
		runCommand("haxe", ["build.hxml"]);
		runCommand("node", ["test.js"]);

		changeDirectory(sysDir);
		runCommand("npm", ["install", "deasync"], true);
		runCommand("haxe", ["compile-js.hxml"].concat(args));
		runSysTest("node", ["bin/js/sys.js"]);

		changeDirectory(miscJsDir);
		runCommand("haxe", ["run.hxml"]);
	}
}
