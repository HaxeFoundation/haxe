using StringTools;

import yaml.*;

import sys.*;
import sys.io.*;

private typedef TravisConfig = {
	before_install: Array<String>,
	script: Array<String>
}

/**
	Will be run by TravisCI.
	See ".travis.yml" at project root for TravisCI settings.
*/
class RunTravis {
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
			exitCode = Sys.command(cmd, args);
			Sys.println('Command exited with $exitCode: $cmd $args');

			if (exitCode == 0) {
				return;
			} else if (trials > 0) {
				Sys.println('Command will be re-run...');
			}
		}

		Sys.exit(exitCode);
	}

	static function getHaxelibPath(libName:String) {
		var proc = new sys.io.Process("haxelib", ["path", libName]);
		var result;
		var code = proc.exitCode();
		while(true) {
			result = proc.stdout.readLine();
			if (!result.startsWith("-L")) {
				break;
			}
		}
		proc.close();
		if (code != 0) {
			Sys.println(result);
			Sys.exit(code);
		}
		trace('Haxelib path for $libName: $result');
		return result;
	}

	static function changeDirectory(path:String) {
		Sys.println('Changing directory to $path');
		Sys.setCwd(path);
	}

	static function setupFlashPlayerDebugger():Void {
		Sys.putEnv("DISPLAY", ":99.0");
		runCommand("sh", ["-e", "/etc/init.d/xvfb", "start"]);
		Sys.putEnv("AUDIODEV", "null");
		runCommand("sudo", ["apt-get", "install", "-qq", "libgd2-xpm", "ia32-libs", "ia32-libs-multiarch", "-y"], true);
		runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/11/flashplayer_11_sa_debug.i386.tar.gz"], true);
		runCommand("tar", ["-xf", "flashplayer_11_sa_debug.i386.tar.gz", "-C", Sys.getEnv("HOME")]);
		File.saveContent(Sys.getEnv("HOME") + "/mm.cfg", "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
		runCommand(Sys.getEnv("HOME") + "/flashplayerdebugger", ["-v"]);
	}

	static function runFlash(swf:String):Void {
		Sys.command(Sys.getEnv("HOME") + "/flashplayerdebugger", [swf, "&"]);

		//wait a little until flashlog.txt is created
		var flashlogPath = Sys.getEnv("HOME") + "/.macromedia/Flash_Player/Logs/flashlog.txt";
		for (t in 0...5) {
			runCommand("sleep", ["2"]);
			if (FileSystem.exists(flashlogPath))
				break;
		}
		if (!FileSystem.exists(flashlogPath)) {
			Sys.println('$flashlogPath not found.');
			Sys.exit(1);
		}

		//read flashlog.txt continously
		var traceProcess = new Process("tail", ["-f", "-v", flashlogPath]);
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

	static function parseTravisFile(path:String, ignoreBeforeInstall = false) {
		var yaml:TravisConfig = yaml.Yaml.read(path, Parser.options().useObjects());
		if (!ignoreBeforeInstall) {
			for (code in yaml.before_install) {
				var args = parseCommand(code);
				var cmd = args.shift();
				runCommand(cmd, args);
			}
		}
		for (code in yaml.script) {
			var args = parseCommand(code);
			var cmd = args.shift();
			runCommand(cmd, args);
		}
	}

	static function getPhpDependencies() {
		runCommand("sudo", ["apt-get", "install", "php5", "-y"], true);
	}

	static function getCppDependencies(unitDir:String) {
		//hxcpp dependencies
		runCommand("sudo", ["apt-get", "install", "gcc-multilib", "g++-multilib", "-y"], true);

		//install and build hxcpp
		runCommand("haxelib", ["git", "hxcpp", "https://github.com/HaxeFoundation/hxcpp.git"], true);
		Sys.setCwd(Sys.getEnv("HOME") + "/haxelib/hxcpp/git/project/");
		runCommand("neko", ["build.n"]);
		Sys.setCwd(unitDir);
	}

	static function getJavaDependencies() {
		runCommand("haxelib", ["git", "hxjava", "https://github.com/HaxeFoundation/hxjava.git"], true);
	}

	static function getCsDependencies() {
		runCommand("sudo", ["apt-get", "install", "mono-devel", "mono-mcs", "-y"], true);
		runCommand("haxelib", ["git", "hxcs", "https://github.com/HaxeFoundation/hxcs.git"], true);
	}

	static function main():Void {
		var cwd = Sys.getCwd();
		var unitDir = cwd + "unit/";
		var optDir = cwd + "optimization/";

		Sys.setCwd(unitDir);
		switch (Sys.getEnv("TARGET")) {
			case "macro", null:
				runCommand("haxe", ["compile-macro.hxml"]);

				//generate documentation
				runCommand("haxelib", ["git", "hxparse", "https://github.com/Simn/hxparse", "development", "src"], true);
				runCommand("haxelib", ["git", "hxtemplo", "https://github.com/Simn/hxtemplo", "master", "src"], true);
				runCommand("haxelib", ["git", "hxargs", "https://github.com/Simn/hxargs.git"], true);
				runCommand("haxelib", ["git", "markdown", "https://github.com/dpeek/haxe-markdown.git", "master", "src"], true);

				runCommand("haxelib", ["git", "hxcpp", "https://github.com/HaxeFoundation/hxcpp.git"], true);
				runCommand("haxelib", ["git", "hxjava", "https://github.com/HaxeFoundation/hxjava.git"], true);
				runCommand("haxelib", ["git", "hxcs", "https://github.com/HaxeFoundation/hxcs.git"], true);

				runCommand("haxelib", ["git", "dox", "https://github.com/dpeek/dox.git"], true);
				Sys.setCwd(Sys.getEnv("HOME") + "/haxelib/dox/git/");
				runCommand("haxe", ["run.hxml"]);
				runCommand("haxe", ["gen.hxml"]);
				runCommand("haxelib", ["run", "dox", "-o", "bin/api.zip", "-i", "bin/xml"]);
			case "neko":
				runCommand("haxe", ["compile-neko.hxml"]);
				runCommand("neko", ["unit.n"]);
			case "php":
				getPhpDependencies();
				runCommand("haxe", ["compile-php.hxml"]);
				runCommand("php", ["php/index.php"]);
			case "cpp":
				getCppDependencies(unitDir);
				runCommand("haxe", ["compile-cpp.hxml"]);
				runCommand("./cpp/Test-debug", []);

				runCommand("rm", ["-rf", "cpp"]);

				runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M64"]);
				runCommand("./cpp/Test-debug", []);
			case "js":
				runCommand("haxe", ["compile-js.hxml"]);
				runCommand("node", ["-e", "var unit = require('./unit.js').unit; unit.Test.main(); process.exit(unit.Test.success ? 0 : 1);"]);

				if (Sys.getEnv("TRAVIS_SECURE_ENV_VARS") == "true") {
					//https://saucelabs.com/opensource/travis
					runCommand("npm", ["install", "wd"], true);
					runCommand("curl", ["https://gist.github.com/santiycr/5139565/raw/sauce_connect_setup.sh", "-L", "|", "bash"], true);
					runCommand("haxelib", ["git", "nodejs", "https://github.com/dionjwa/nodejs-std.git", "master", "src"], true);
					runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
					runCommand("nekotools", ["server", "&"]);
					runCommand("node", ["RunSauceLabs.js"]);
				}

				Sys.println("Test optimization:");
				Sys.setCwd(optDir);
				runCommand("haxe", ["run.hxml"]);
			case "java":
				getJavaDependencies();
				runCommand("haxe", ["compile-java.hxml"]);
				runCommand("java", ["-jar", "java/Test-Debug.jar"]);
			case "cs":
				getCsDependencies();

				runCommand("haxe", ["compile-cs.hxml"]);
				runCommand("mono", ["cs/bin/Test-Debug.exe"]);

				runCommand("haxe", ["compile-cs-unsafe.hxml"]);
				runCommand("mono", ["cs_unsafe/bin/Test-Debug.exe"]);
			case "flash9":
				setupFlashPlayerDebugger();
				runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb"]);
				runFlash("unit9.swf");
			case "flash8":
				setupFlashPlayerDebugger();
				runCommand("haxe", ["compile-flash8.hxml", "-D", "fdb"]);
				runFlash("unit8.swf");
			case "as3":
				setupFlashPlayerDebugger();

				//setup flex sdk
				runCommand("wget", ["http://mirror.cc.columbia.edu/pub/software/apache/flex/4.12.0/binaries/apache-flex-sdk-4.12.0-bin.tar.gz"], true);
				runCommand("tar", ["-xf", "apache-flex-sdk-4.12.0-bin.tar.gz", "-C", Sys.getEnv("HOME")]);
				var flexsdkPath = Sys.getEnv("HOME") + "/apache-flex-sdk-4.12.0-bin";
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ":" + flexsdkPath + "/bin");
				var playerglobalswcFolder = flexsdkPath + "/player";
				FileSystem.createDirectory(playerglobalswcFolder + "/11.1");
				runCommand("wget", ["-nv", "http://download.macromedia.com/get/flashplayer/updaters/11/playerglobal11_1.swc", "-O", playerglobalswcFolder + "/11.1/playerglobal.swc"], true);
				File.saveContent(flexsdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerglobalswcFolder');
				runCommand("mxmlc", ["--version"]);

				runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"]);
				runFlash("unit9_as3.swf");
			case "openfl-samples":
				getCppDependencies(unitDir);
				runCommand("haxelib", ["git", "hxlibc", "https://github.com/openfl/hxlibc"]);
				runCommand("haxelib", ["git", "actuate", "https://github.com/jgranick/actuate"]);
				runCommand("haxelib", ["git", "box2d", "https://github.com/jgranick/box2d"]);
				runCommand("haxelib", ["git", "swf", "https://github.com/openfl/swf"]);
				runCommand("haxelib", ["git", "layout", "https://github.com/jgranick/layout"]);
				runCommand("haxelib", ["git", "format", "https://github.com/HaxeFoundation/format"]);
				runCommand("haxelib", ["git", "svg", "https://github.com/openfl/svg"]);
				runCommand("haxelib", ["git", "lime", "https://github.com/openfl/lime"]);
				runCommand("haxelib", ["git", "lime-build", "https://github.com/openfl/lime-build"]);
				runCommand("haxelib", ["git", "lime-tools", "https://github.com/openfl/lime-tools"]);
				runCommand("haxelib", ["git", "openfl-native", "https://github.com/openfl/openfl-native"]);
				runCommand("haxelib", ["git", "openfl", "https://github.com/openfl/openfl"]);
				runCommand("haxelib", ["git", "openfl-samples", "https://github.com/Simn/openfl-samples"]);
				runCommand("haxelib", ["run", "openfl", "rebuild", "linux"]);
				runCommand("haxelib", ["run", "openfl", "rebuild", "tools"]);
				var path = getHaxelibPath("openfl-samples");
				var old = Sys.getEnv("pwd");
				Sys.putEnv("pwd", path);
				parseTravisFile(haxe.io.Path.join([path, ".travis.yml"]), true);
				if (old != null) {
					Sys.putEnv("pwd", old);
				}
			case "polygonal-ds":
				runCommand("haxelib", ["git", "polygonal-ds", "https://github.com/Simn/ds"]);
				runCommand("haxelib", ["git", "polygonal-core", "https://github.com/polygonal/core", "master", "src"]);
				runCommand("haxelib", ["git", "polygonal-printf", "https://github.com/polygonal/printf", "master", "src"]);
				changeDirectory(getHaxelibPath("polygonal-ds"));
				runCommand("haxe", ["-cp", "src", "-cp", "test", "-lib", "polygonal-core", "-lib", "polygonal-printf", "-main", "UnitTest", "-js", "unit.js"]);
				runCommand("node", ["unit.js"]);
			case "flambe":
				runCommand("git", ["clone", "https://github.com/aduros/flambe"]);
				runCommand("sh", ["flambe/bin/run-travis"]);
			case "hxtemplo":
				getJavaDependencies();
				getPhpDependencies();
				getCppDependencies(unitDir);
				runCommand("haxelib", ["git", "hxparse", "https://github.com/Simn/hxparse", "development", "src"]);
				runCommand("haxelib", ["git", "hxtemplo", "https://github.com/Simn/hxtemplo"]);

				changeDirectory(getHaxelibPath("hxtemplo"));
				runCommand("haxe", ["build.hxml"]);

				runCommand("node", ["bin/hxtemplo.js"]);
				runCommand("neko", ["bin/hxtemplo.n"]);
				runCommand("java", ["-jar", "bin/java/Test.jar"]);
				runCommand("php", ["bin/php/index.php"]);
				runCommand("./bin/cpp/Test", []);
			case "munit":
				runCommand("haxelib", ["git", "mconsole", "https://github.com/massiveinteractive/mconsole", "master", "src"]);
				runCommand("haxelib", ["git", "mcover", "https://github.com/massiveinteractive/MassiveCover", "master", "src"]);
				runCommand("haxelib", ["git", "mlib", "https://github.com/massiveinteractive/MassiveLib", "master", "src"]);
				runCommand("haxelib", ["git", "munit", "https://github.com/massiveinteractive/MassiveUnit", "master", "src"]);
				changeDirectory(haxe.io.Path.join([getHaxelibPath("munit"), "..", "tool"]));
				runCommand("haxe", ["build.hxml"]);
				runCommand("haxelib", ["run", "munit", "test", "-result-exit-code", "-neko"]);
				changeDirectory("../");
				runCommand("haxelib", ["run", "munit", "test", "-result-exit-code", "-neko"]);
			case target:
				throw "unknown target: " + target;
		}
	}
}
