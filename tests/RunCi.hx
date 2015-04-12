using StringTools;

import yaml.*;

import sys.*;
import sys.io.*;
import haxe.*;
import haxe.io.*;

private typedef TravisConfig = {
	before_install: Array<String>,
	script: Array<String>
}

/**
	List of "TEST" defined in the "matrix" section of ".travis.yml".
*/
@:enum abstract TEST(String) from String {
	var Macro = "macro";
	var Neko = "neko";
	var Js = "js";
	var Php = "php";
	var Cpp = "cpp";
	var Flash9 = "flash9";
	var As3 = "as3";
	var Java = "java";
	var Cs = "cs";
	var Python = "python";
	var ThirdParty = "third-party";
}

enum Ci {
	TravisCI;
	AppVeyor;
}

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
	static function successMsg(msg:String):Void {
		Sys.println('\x1b[32m' + msg + '\x1b[0m');
	}
	static function failMsg(msg:String):Void {
		Sys.println('\x1b[31m' + msg + '\x1b[0m');
	}
	static function infoMsg(msg:String):Void {
		Sys.println('\x1b[36m' + msg + '\x1b[0m');
	}

	/**
		Run a command using `Sys.command()`.
		If the command exits with non-zero code, exit the whole script with the same code.

		If `useRetry` is `true`, the command will be re-run if it exits with non-zero code (3 trials).
		It is useful for running network-dependent commands.
	*/
	static function runCommand(cmd:String, args:Array<String>, useRetry:Bool = false):Void {
		var trials = useRetry ? 3 : 1;
		var exitCode:Int = 1;

		while (trials-->0) {
			Sys.println('Command: $cmd $args');

			var t = Timer.stamp();
			exitCode = Sys.command(cmd, args);
			var dt = Math.round(Timer.stamp() - t);

			if (exitCode == 0)
				successMsg('Command exited with $exitCode in ${dt}s: $cmd $args');
			else
				failMsg('Command exited with $exitCode in ${dt}s: $cmd $args');

			if (exitCode == 0) {
				return;
			} else if (trials > 0) {
				Sys.println('Command will be re-run...');
			}
		}

		Sys.exit(exitCode);
	}

	static function haxelibInstallGit(account:String, repository:String, ?branch:String, ?srcPath:String, useRetry:Bool = false, ?altName:String):Void {
		var name:String = (altName == null) ? repository : altName;
		try {
			getHaxelibPath(name);
			infoMsg('$name has already been installed.');
		} catch (e:Dynamic) {
			var args:Array<String> = ["git", name, 'https://github.com/$account/$repository'];
			if (branch != null) {
				args.push(branch);
			}
			if (srcPath != null) {
				args.push(srcPath);
			}

			runCommand("haxelib", args, useRetry);
		}
	}

	static function haxelibInstall(library:String):Void {
		try {
			getHaxelibPath(library);
			infoMsg('$library has already been installed.');
		} catch (e:Dynamic) {
			runCommand("haxelib", ["install", library]);
		}
	}

	static function haxelibRun(args:Array<String>, useRetry:Bool = false):Void {
		runCommand("haxelib", ["run"].concat(args), useRetry);
	}

	static function getHaxelibPath(libName:String) {
		var proc = new Process("haxelib", ["path", libName]);
		var result;
		var code = proc.exitCode();
		do {
			result = proc.stdout.readLine();
			if (!result.startsWith("-L")) {
				break;
			}
		} while(true);
		proc.close();
		if (code != 0) {
			throw 'Failed to get haxelib path ($result)';
		}
		return result;
	}

	static function changeDirectory(path:String) {
		Sys.println('Changing directory to $path');
		Sys.setCwd(path);
	}

	static function setupFlashPlayerDebugger():Void {
		var mmcfgPath = switch (systemName) {
			case "Linux":
				Sys.getEnv("HOME") + "/mm.cfg";
			case "Mac":
				"/Library/Application Support/Macromedia/mm.cfg";
			case _:
				throw "unsupported system";
		}

		switch (systemName) {
			case "Linux":
				Sys.putEnv("DISPLAY", ":99.0");
				runCommand("sh", ["-e", "/etc/init.d/xvfb", "start"]);
				Sys.putEnv("AUDIODEV", "null");
				runCommand("sudo", ["apt-get", "install", "-qq", "libgd2-xpm", "ia32-libs", "ia32-libs-multiarch"], true);
				runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/11/flashplayer_11_sa_debug.i386.tar.gz"], true);
				runCommand("tar", ["-xf", "flashplayer_11_sa_debug.i386.tar.gz", "-C", Sys.getEnv("HOME")]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				runCommand(Sys.getEnv("HOME") + "/flashplayerdebugger", ["-v"]);
			case "Mac":
				runCommand("brew", ["cask", "install", "flash-player-debugger"]);
				var dir = Path.directory(mmcfgPath);
				runCommand("sudo", ["mkdir", "-p", dir]);
				runCommand("sudo", ["chmod", "a+w", dir]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
		}
	}

	static function runFlash(swf:String):Void {
		swf = FileSystem.fullPath(swf);
		Sys.println('going to run $swf');
		switch (systemName) {
			case "Linux":
				new Process(Sys.getEnv("HOME") + "/flashplayerdebugger", [swf]);
			case "Mac":
				Sys.command("open", ["-a", Sys.getEnv("HOME") + "/Applications/Flash Player Debugger.app", swf]);
		}

		//wait a little until flashlog.txt is created
		var flashlogPath = switch (systemName) {
			case "Linux":
				Sys.getEnv("HOME") + "/.macromedia/Flash_Player/Logs/flashlog.txt";
			case "Mac":
				Sys.getEnv("HOME") + "/Library/Preferences/Macromedia/Flash Player/Logs/flashlog.txt";
			case _:
				throw "unsupported system";
		}

		for (t in 0...5) {
			runCommand("sleep", ["2"]);
			if (FileSystem.exists(flashlogPath))
				break;
		}
		if (!FileSystem.exists(flashlogPath)) {
			failMsg('$flashlogPath not found.');
			Sys.exit(1);
		}

		//read flashlog.txt continously
		var traceProcess = new Process("tail", ["-f", flashlogPath]);
		var line = "";
		while (true) {
			try {
				line = traceProcess.stdout.readLine();
				Sys.println(line);
				if (line.indexOf("SUCCESS: ") >= 0) {
					Sys.exit(line.indexOf("SUCCESS: true") >= 0 ? 0 : 1);
				}
			} catch (e:haxe.io.Eof) {}
		}
		Sys.exit(1);
	}

	static function runCs(exe:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		exe = FileSystem.fullPath(exe);
		switch (systemName) {
			case "Linux", "Mac":
				runCommand("mono", [exe].concat(args));
			case "Windows":
				runCommand(exe, args);
		}
	}

	static function runCpp(bin:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		bin = FileSystem.fullPath(bin);
		runCommand(bin, args);
	}

	static function parseCommand(cmd:String) {
		var args = [];
		var offset = 0;
		var cur = new StringBuf();
		var inString = false;

		while(true) {
			switch(cmd.fastCodeAt(offset++)) {
				case '"'.code:
					inString = !inString;
				case ' '.code if (!inString):
					if (cur.length > 0) {
						args.push(cur.toString());
						cur = new StringBuf();
					}
				case '\\'.code:
					cur.addChar(cmd.fastCodeAt(offset++));
				case "$".code:
					switch (cmd.fastCodeAt(offset)) {
						case '('.code:
							++offset;
							var env = new StringBuf();
							while(true) {
								switch(cmd.fastCodeAt(offset++)) {
									case ')'.code:
										break;
									case c:
										env.addChar(c);
								}
							}
							cur.add(Sys.getEnv(env.toString()));
						case _:
							cur.addChar("$".code);
					}
				case c:
					cur.addChar(c);
			}
			if (offset == cmd.length) {
				break;
			}
		}
		if (cur.length > 0) {
			args.push(cur.toString());
		}
		return args;
	}

	//static function parseTravisFile(path:String, ignoreBeforeInstall = false) {
		//var yaml:TravisConfig = yaml.Yaml.read(path, Parser.options().useObjects());
		//if (!ignoreBeforeInstall) {
			//for (code in yaml.before_install) {
				//var args = parseCommand(code);
				//var cmd = args.shift();
				//runCommand(cmd, args);
			//}
		//}
		//for (code in yaml.script) {
			//var args = parseCommand(code);
			//var cmd = args.shift();
			//runCommand(cmd, args);
		//}
	//}

	static function commandSucceed(cmd:String, args:Array<String>):Bool {
		return try {
			new Process(cmd, args).exitCode() == 0;
		} catch(e:Dynamic) false;
	}

	static function addToPATH(path:String):Void {
		switch (systemName) {
			case "Windows":
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ";" + path);
			case "Mac", "Linux":
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ":" + path);
		}
	}

	static function getPhpDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("php", ["-v"])) {
					infoMsg('php has already been installed.');
				} else {
					runCommand("sudo", ["apt-get", "install", "php5", "-qq"], true);
					runCommand("sudo", ["apt-get", "install", "php5-mysql", "php5-sqlite", "-qq"], true);
				}
			case "Mac":
				//pass
			case "Windows":
				if (commandSucceed("php", ["-v"])) {
					infoMsg('php has already been installed.');
				} else {
					runCommand("cinst", ["php", "-version", "5.6.3", "-y"], true);
					addToPATH("C:\\tools\\php");
				}
		}
		runCommand("php", ["-v"]);
	}

	static var gotCppDependencies = false;
	static function getCppDependencies() {
		if (gotCppDependencies) return;

		//hxcpp dependencies
		switch (systemName) {
			case "Linux":
				runCommand("sudo", ["apt-get", "install", "gcc-multilib", "g++-multilib", "-qq"], true);
			case "Mac":
				//pass
		}


		//install and build hxcpp
		try {
			getHaxelibPath("hxcpp");
			infoMsg('hxcpp has already been installed.');
		} catch(e:Dynamic) {
			haxelibInstallGit("HaxeFoundation", "hxcpp", true);
			var oldDir = Sys.getCwd();
			changeDirectory(getHaxelibPath("hxcpp") + "tools/hxcpp/");
			runCommand("haxe", ["compile.hxml"]);
			changeDirectory(getHaxelibPath("hxcpp") + "project/");
			switch (ci) {
				case AppVeyor:
					runCommand("neko", ["build.n", "windows-m32"]);
				case _:
					runCommand("neko", ["build.n"]);
			}
			changeDirectory(oldDir);
		}

		gotCppDependencies = true;
	}

	static function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);

		runCommand("javac", ["-version"]);
	}

	static function getJSDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					runCommand("sudo", ["apt-get", "install", "nodejs", "-qq"], true);
				}
			case "Mac":
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static function getCsDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					runCommand("sudo", ["apt-get", "install", "mono-devel", "mono-mcs", "-qq"], true);
				runCommand("mono", ["--version"]);
			case "Mac":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					runCommand("brew", ["install", "mono"], true);
				runCommand("mono", ["--version"]);
			case "Windows":
				//pass
		}

		haxelibInstallGit("HaxeFoundation", "hxcs", true);
	}

	static var gotOpenFLDependencies = false;
	static function getOpenFLDependencies() {
		if (gotOpenFLDependencies) return;

		getCppDependencies();

		haxelibInstallGit("HaxeFoundation", "format");
		haxelibInstallGit("haxenme", "nme");
		haxelibInstallGit("haxenme", "nme-dev");
		haxelibInstallGit("openfl", "svg");
		haxelibInstallGit("openfl", "lime");
		haxelibInstallGit("openfl", "lime-tools");
		haxelibInstallGit("openfl", "openfl-native");
		haxelibInstallGit("openfl", "openfl-html5");
		haxelibInstallGit("openfl", "openfl");

		switch (systemName) {
			case "Linux":
				haxelibRun(["openfl", "rebuild", "linux"]);
			case "Mac":
				haxelibRun(["openfl", "rebuild", "mac"]);
		}
		haxelibRun(["openfl", "rebuild", "tools"]);

		gotOpenFLDependencies = true;
	}

	/**
		Install python and return the names of the installed pythons.
	*/
	static function getPythonDependencies():Array<String> {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runCommand("sudo", ["apt-get", "install", "python3", "-qq"], true);
				runCommand("python3", ["-V"]);

				var pypy = "pypy3";
				if (commandSucceed(pypy, ["-V"])) {
					infoMsg('pypy3 has already been installed.');
				} else {
					var pypyVersion = "pypy3-2.4.0-linux64";
					runCommand("wget", ['https://bitbucket.org/pypy/pypy/downloads/${pypyVersion}.tar.bz2'], true);
					runCommand("tar", ["-xf", '${pypyVersion}.tar.bz2']);
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
		}

		return [];
	}

	static var ci(default, never):Null<Ci> =
		if (Sys.getEnv("TRAVIS") == "true")
			TravisCI;
		else if (Sys.getEnv("APPVEYOR") == "True")
			AppVeyor;
		else
			null;
	static var systemName(default, never) = Sys.systemName();
	static var cwd(default, never) = Sys.getCwd();
	static var repoDir(default, never) = FileSystem.fullPath("..") + "/";
	static var unitDir(default, never) = cwd + "unit/";
	static var sysDir(default, never) = cwd + "sys/";
	static var optDir(default, never) = cwd + "optimization/";
	static var miscDir(default, never) = cwd + "misc/";

	static function main():Void {
		Sys.putEnv("OCAMLRUNPARAM", "b");

		var tests:Array<TEST> = switch (Sys.getEnv("TEST")) {
			case null:
				[Macro];
			case env:
				[for (v in env.split(",")) v.trim().toLowerCase()];
		}
		Sys.println('Going to test: $tests');

		for (test in tests) {
			infoMsg('Now test $test');
			changeDirectory(unitDir);
			switch (test) {
				case Macro:
					runCommand("haxe", ["compile-macro.hxml"]);

					changeDirectory(miscDir);
					getCsDependencies();
					runCommand("haxe", ["compile.hxml"]);

					switch (ci) {
						case AppVeyor:
							//save time...
						case _:
							//generate documentation
							haxelibInstallGit("Simn", "hxparse", "development", "src", true);
							haxelibInstallGit("Simn", "hxtemplo", true);
							haxelibInstallGit("Simn", "hxargs", true);
							haxelibInstallGit("dpeek", "haxe-markdown", "master", "src", true, "markdown");

							haxelibInstallGit("HaxeFoundation", "hxcpp", true);
							haxelibInstallGit("HaxeFoundation", "hxjava", true);
							haxelibInstallGit("HaxeFoundation", "hxcs", true);

							haxelibInstallGit("dpeek", "dox", true);
							changeDirectory(getHaxelibPath("dox"));
							runCommand("haxe", ["run.hxml"]);
							runCommand("haxe", ["gen.hxml"]);
							haxelibRun(["dox", "-o", "bin/api.zip", "-i", "bin/xml"]);
					}

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-macro.hxml"]);
					runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);

					//BYTECODE
					switch (ci) {
						case null:
							//pass
						case TravisCI:
							changeDirectory(repoDir);
							runCommand("make", ["BYTECODE=1", "-s"]);
							runCommand("sudo", ["make", "install", "-s"]);
							changeDirectory(unitDir);
							runCommand("haxe", ["compile-macro.hxml"]);
						case AppVeyor:
							// save time...
							// changeDirectory(repoDir);
							// runCommand(Sys.getEnv("CYG_ROOT") + "/bin/bash", ["-lc", 'cd \"$$OLDPWD\" && make -s -f Makefile.win BYTECODE=1']);
							// changeDirectory(unitDir);
							// runCommand("haxe", ["compile-macro.hxml"]);
					}
				case Neko:
					runCommand("haxe", ["compile-neko.hxml"]);
					runCommand("neko", ["bin/unit.n"]);

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-neko.hxml"]);
					runCommand("neko", ["bin/neko/sys.n"]);
				case Php:
					getPhpDependencies();
					var args = switch (ci) {
						case TravisCI:
							["-D","travis"];
						case _:
							[];
					}
					runCommand("haxe", ["compile-php.hxml"].concat(args));
					runCommand("php", ["bin/php/index.php"]);

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-php.hxml"]);
					runCommand("php", ["bin/php/Main/index.php"]);
				case Python:
					var pys = getPythonDependencies();

					runCommand("haxe", ["compile-python.hxml"]);
					for (py in pys) {
						runCommand(py, ["bin/unit.py"]);
					}

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-python.hxml"]);
					for (py in pys) {
						runCommand(py, ["bin/python/sys.py"]);
					}

					changeDirectory(miscDir + "pythonImport");
					runCommand("haxe", ["compile.hxml"]);
					for (py in pys) {
						runCommand(py, ["test.py"]);
					}
				case Cpp:
					getCppDependencies();
					runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M32"]);
					runCpp("bin/cpp/Test-debug", []);

					switch (ci) {
						case AppVeyor:
							//save time...
						case _:
							runCommand("rm", ["-rf", "cpp"]);
							runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M64"]);
							runCpp("bin/cpp/Test-debug", []);
					}

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-cpp.hxml"]);
					runCpp("bin/cpp/Main-debug", []);
				case Js:
					getJSDependencies();

					for (flatten in [true, false]) {
						runCommand("haxe", ["compile-js.hxml"].concat(flatten ? [] : ["-D", "js-unflatten"]));
						runCommand("node", ["-e", "var unit = require('./bin/unit.js').unit; unit.Test.main(); process.exit(unit.Test.success ? 0 : 1);"]);
					}

					if (Sys.getEnv("TRAVIS_SECURE_ENV_VARS") == "true" && systemName == "Linux") {
						var scVersion = "sc-4.3-linux";
						runCommand("wget", ['https://saucelabs.com/downloads/${scVersion}.tar.gz'], true);
						runCommand("tar", ["-xf", '${scVersion}.tar.gz']);

						//start sauce-connect
						var scReadyFile = "sauce-connect-ready-" + Std.random(100);
						var sc = new Process('${scVersion}/bin/sc', [
							"-i", Sys.getEnv("TRAVIS_JOB_NUMBER"),
							"-f", scReadyFile
						]);
						while(!FileSystem.exists(scReadyFile)) {
							Sys.sleep(0.5);
						}

						runCommand("npm", ["install", "wd", "q"], true);
						haxelibInstallGit("dionjwa", "nodejs-std", "master", null, true, "nodejs");
						runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
						var server = new Process("nekotools", ["server"]);
						runCommand("node", ["bin/RunSauceLabs.js", "unit-js.html"]);

						server.close();
						sc.close();
					}

					infoMsg("Test optimization:");
					changeDirectory(optDir);
					runCommand("haxe", ["run.hxml"]);
				case Java:
					getJavaDependencies();
					runCommand("haxe", ["compile-java.hxml"]);
					runCommand("java", ["-jar", "bin/java/Test-Debug.jar"]);

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-java.hxml"]);
					runCommand("java", ["-jar", "bin/java/Main-Debug.jar"]);

					infoMsg("Testing java-lib extras");
					changeDirectory('$unitDir/bin');
					runCommand("git", ["clone", "https://github.com/waneck/java-lib-tests.git", "--depth", "1"], true);
					for (dir in FileSystem.readDirectory('java-lib-tests'))
					{
						var path = 'java-lib-tests/$dir';
						if (FileSystem.isDirectory(path)) for (file in FileSystem.readDirectory(path))
						{
							if (file.endsWith('.hxml'))
							{
								runCommand("haxe", ["--cwd",'java-lib-tests/$dir',file]);
							}
						}
					}

				case Cs:
					getCsDependencies();

					var compl = switch [ci, systemName] {
						case [TravisCI, "Linux"]:
							"-travis";
						case _:
							"";
					};

					runCommand("haxe", ['compile-cs$compl.hxml']);
					runCs("bin/cs/bin/Test-Debug.exe");

					runCommand("haxe", ['compile-cs-unsafe$compl.hxml']);
					runCs("bin/cs_unsafe/bin/Test-Debug.exe");

					runCommand("haxe", ['compile-cs$compl.hxml',"-D","erase_generics"]);
					runCs("bin/cs/bin/Test-Debug.exe");

					runCommand("haxe", ['compile-cs-unsafe$compl.hxml',"-D","erase_generics"]);
					runCs("bin/cs_unsafe/bin/Test-Debug.exe");

					runCommand("haxe", ['compile-cs$compl.hxml',"-D","no_root"]);
					runCs("bin/cs/bin/Test-Debug.exe");

					runCommand("haxe", ['compile-cs-unsafe$compl.hxml',"-D","no_root","-D","erase_generics"]);
					runCs("bin/cs_unsafe/bin/Test-Debug.exe");

					changeDirectory(sysDir);
					runCommand("haxe", ["compile-cs.hxml"]);
					runCs("bin/cs/bin/Main-Debug.exe", []);

					changeDirectory(miscDir + "csTwoLibs");
					for (i in 1...5)
					{
						runCommand("haxe", ['compile-$i.hxml']);
						runCs("bin/main/bin/Main.exe");
					}

				case Flash9:
					setupFlashPlayerDebugger();
					runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb"]);
					runFlash("bin/unit9.swf");
				case As3:
					setupFlashPlayerDebugger();

					//setup flex sdk
					if (commandSucceed("mxmlc", ["--version"])) {
						infoMsg('mxmlc has already been installed.');
					} else {
						var flexVersion = "4.14.0";
						runCommand("wget", ['http://archive.apache.org/dist/flex/${flexVersion}/binaries/apache-flex-sdk-${flexVersion}-bin.tar.gz'], true);
						runCommand("tar", ["-xf", 'apache-flex-sdk-${flexVersion}-bin.tar.gz', "-C", Sys.getEnv("HOME")]);
						var flexsdkPath = Sys.getEnv("HOME") + '/apache-flex-sdk-${flexVersion}-bin';
						addToPATH(flexsdkPath + "/bin");
						var playerglobalswcFolder = flexsdkPath + "/player";
						FileSystem.createDirectory(playerglobalswcFolder + "/11.1");
						runCommand("wget", ["-nv", "http://download.macromedia.com/get/flashplayer/updaters/11/playerglobal11_1.swc", "-O", playerglobalswcFolder + "/11.1/playerglobal.swc"], true);
						File.saveContent(flexsdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerglobalswcFolder');
						runCommand("mxmlc", ["--version"]);
					}

					runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"]);
					runFlash("bin/unit9_as3.swf");
				case ThirdParty:
					getPhpDependencies();
					getJavaDependencies();
					getJSDependencies();
					getCsDependencies();
					getPythonDependencies();
					getCppDependencies();
					//getOpenFLDependencies();

					//testPolygonalDs();
					// if (systemName == "Linux") testFlambe(); //#3439
					testHxTemplo();
					testMUnit();
					//testOpenflSamples();
					//testFlixelDemos();
				case t:
					throw "unknown target: " + t;
			}
		}
	}

	static function testHxTemplo() {
		infoMsg("Test hx-templo:");

		changeDirectory(unitDir);

		haxelibInstallGit("Simn", "hxparse", "development", "src");
		haxelibInstallGit("Simn", "hxtemplo");

		var buildArgs = [
			"-cp", "src",
			"-cp", "test",
			"-main", "Test",
			"-lib", "hxparse",
			"-dce", "full"
		];

		changeDirectory(getHaxelibPath("hxtemplo") + "..");
		runCommand("haxe", ["build.hxml"]);
	}

	static function testPolygonalDs() {
		infoMsg("Test polygonal-ds:");

		changeDirectory(unitDir);
		haxelibInstallGit("Simn", "ds", "python-support", null, false, "polygonal-ds");
		haxelibInstallGit("polygonal", "core", "master", "src", false, "polygonal-core");
		haxelibInstallGit("polygonal", "printf", "master", "src", false, "polygonal-printf");
		changeDirectory(getHaxelibPath("polygonal-ds"));
		runCommand("haxe", ["build.hxml"]);
		runCommand("python3", ["unit.py"]);
		runCommand("node", ["unit.js"]);
	}

	static function testMUnit() {
		infoMsg("Test MUnit:");

		changeDirectory(unitDir);

		haxelibInstallGit("massiveinteractive", "mconsole", "master", "src");
		haxelibInstallGit("massiveinteractive", "MassiveCover", "master", "src", false, "mcover");
		haxelibInstallGit("massiveinteractive", "MassiveLib", "master", "src", false, "mlib");
		haxelibInstallGit("massiveinteractive", "MassiveUnit", "master", "src", false, "munit");
		changeDirectory(Path.join([getHaxelibPath("munit"), "..", "tool"]));
		runCommand("haxe", ["build.hxml"]);
		haxelibRun(["munit", "test", "-result-exit-code", "-neko"], true);
		changeDirectory("../");
		haxelibRun(["munit", "test", "-result-exit-code", "-neko"], true);
	}

	static function testFlambe() {
		infoMsg("Test Flambe:");

		changeDirectory(unitDir);
		runCommand("git", ["clone", "https://github.com/aduros/flambe"]);
		runCommand("sh", ["flambe/bin/run-travis"]);
	}

	//static function testOpenflSamples() {
		//infoMsg("Test OpenFL Samples:");
//
		//changeDirectory(unitDir);
//
		//haxelibInstallGit("jgranick", "actuate");
		//haxelibInstallGit("jgranick", "box2d");
		//haxelibInstallGit("jgranick", "layout");
		//haxelibInstallGit("openfl", "swf");
		//haxelibInstallGit("openfl", "openfl-samples");
//
		//var path = getHaxelibPath("openfl-samples");
		//var old = Sys.getEnv("pwd");
		//Sys.putEnv("pwd", path);
		//parseTravisFile(haxe.io.Path.join([path, ".travis.yml"]), true);
		//if (old != null) {
			//Sys.putEnv("pwd", old);
		//}
	//}

	static function testFlixelDemos() {
		infoMsg("Test Flixel Demos:");

		changeDirectory(unitDir);
		getOpenFLDependencies();

		haxelibInstall("systools");
		haxelibInstall("spinehx");
		haxelibInstall("nape");
		haxelibInstall("task");

		haxelibInstallGit("larsiusprime", "firetongue");
		haxelibInstallGit("YellowAfterLife", "openfl-bitfive");

		haxelibInstallGit("HaxeFlixel", "flixel");
		haxelibInstallGit("HaxeFlixel", "flixel-addons");
		haxelibInstallGit("HaxeFlixel", "flixel-ui");
		haxelibInstallGit("HaxeFlixel", "flixel-demos");
		haxelibInstallGit("HaxeFlixel", "flixel-tools");

		haxelibRun(["flixel-tools", "testdemos", "-flash"]);
		haxelibRun(["flixel-tools", "testdemos", "-neko"]);
		haxelibRun(["flixel-tools", "testdemos", "-html5"]);
	}
}

