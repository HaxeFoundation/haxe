package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Python {
	static public function getPythonDependencies():Array<String> {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					Linux.requireAptPackages(["python3"]);
				runCommand("python3", ["-V"]);

				var pypy = "pypy3";
				if (commandSucceed(pypy, ["-V"])) {
					infoMsg('pypy3 has already been installed.');
				} else {
					var pypyVersion = "pypy3-2.4.0-linux64";
					var file = '${pypyVersion}.tar.bz2';
					if(!FileSystem.exists(file)) {
						runCommand("wget", ['https://bitbucket.org/pypy/pypy/downloads/$file'], true);
					}
					runCommand("tar", ["-xf", file]);
					pypy = FileSystem.fullPath('${pypyVersion}/bin/pypy3');
				}
				runCommand(pypy, ["-V"]);

				return ["python3", pypy];
			case "Mac":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runCommand("brew", ["install", "python3"], true);
				runCommand("python3", ["-V"]);

				if (commandSucceed("pypy3", ["-V"]))
					infoMsg('pypy3 has already been installed.');
				else
					runCommand("brew", ["install", "pypy3"], true);
				runCommand("pypy3", ["-V"]);

				return ["python3", "pypy3"];
			case "Windows":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					throw "please install python 3.x and make it available as python3 in PATH";
				runCommand("python3", ["-V"]);
				return ["python3"];
		}

		return [];
	}

	static public function run(args:Array<String>) {
		var pys = getPythonDependencies();
		runCommand("haxe", ["compile-python.hxml"].concat(args));
		for (py in pys) {
			runCommand(py, ["bin/unit.py"]);
		}

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-python.hxml"]);
		for (py in pys) {
			runCommand(py, ["bin/python/sys.py"]);
		}

		changeDirectory(miscDir + "pythonImport");
		runCommand("haxe", ["compile.hxml"]);
		for (py in pys) {
			runCommand(py, ["test.py"]);
		}
	}
}