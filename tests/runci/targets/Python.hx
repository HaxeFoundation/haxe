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
					final pypyVersion = "pypy3.8-v7.3.7-" + (switch Linux.arch {
						case Arm64: "aarch64";
						case Amd64: "linux64";
					});
					final file = '${pypyVersion}.tar.bz2';
					if(!FileSystem.exists(file)) {
						runNetworkCommand("wget", ["-nv", 'https://downloads.python.org/pypy/$file']);
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
					runNetworkCommand("brew", ["install", "python3"]);
				runCommand("python3", ["-V"]);

				if (commandSucceed("pypy3", ["-V"]))
					infoMsg('pypy3 has already been installed.');
				else
					runNetworkCommand("brew", ["install", "pypy3"]);
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
		final pys = getPythonDependencies();
		runCommand("haxe", ["compile-python.hxml"].concat(args));
		for (py in pys) {
			runCommand(py, ["bin/unit.py"]);
		}

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-python.hxml"].concat(args));
		for (py in pys) {
			runSysTest(py, ["bin/python/sys.py"]);
		}

		changeDirectory(getMiscSubDir("python"));
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(getMiscSubDir('python', "pythonImport"));
		runCommand("haxe", ["compile.hxml"]);
		for (py in pys) {
			runCommand(py, ["test.py"]);
		}

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--python", "export/threads.py"].concat(args));
		for (py in pys) {
			runCommand(py, ["export/threads.py"]);
		}
	}
}
