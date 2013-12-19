import sys.*;
import sys.io.*;

/**
	Will be run by TravisCI.
	See ".travis.yml" at project root for TravisCI settings.
*/
class RunTravis {
	static function runCommand(cmd:String, args:Array<String>):Void {
		var exitCode = Sys.command(cmd, args);
		Sys.println('Command exited with $exitCode: $cmd $args');

		if (exitCode != 0) {
			Sys.exit(1);
		}
	}

	static function setupFlashPlayerDebugger():Void {
		Sys.putEnv("DISPLAY", ":99.0");
		runCommand("sh", ["-e", "/etc/init.d/xvfb", "start"]);
		Sys.putEnv("AUDIODEV", "null");
		runCommand("sudo", ["apt-get", "install", "-qq", "libgd2-xpm", "ia32-libs", "ia32-libs-multiarch", "-y"]);
		runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/11/flashplayer_11_sa_debug.i386.tar.gz"]);
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

	static function main():Void {
		var cwd = Sys.getCwd();
		var unitDir = cwd + "unit/";
		var optDir = cwd + "optimization/";

		Sys.setCwd(unitDir);
		switch (Sys.getEnv("TARGET")) {
			case "macro", null:
				runCommand("haxe", ["compile-macro.hxml"]);
			case "neko":
				runCommand("haxe", ["compile-neko.hxml"]);
				runCommand("neko", ["unit.n"]);
			case "php":
				runCommand("sudo", ["apt-get", "install", "php5", "-y"]);
				runCommand("haxe", ["compile-php.hxml"]);
				runCommand("php", ["php/index.php"]);
			case "cpp":
				//hxcpp dependencies
				runCommand("sudo", ["apt-get", "install", "gcc-multilib", "g++-multilib", "-y"]);

				//install and build hxcpp
				runCommand("haxelib", ["git", "hxcpp", "https://github.com/HaxeFoundation/hxcpp.git"]);
				Sys.setCwd(Sys.getEnv("HOME") + "/haxelib/hxcpp/git/runtime/");
				runCommand("haxelib", ["run", "hxcpp", "BuildLibs.xml"]);
				Sys.setCwd(unitDir);
				
				runCommand("haxe", ["compile-cpp.hxml"]);
				runCommand("./cpp/Test-debug", []);
			case "js":
				runCommand("haxe", ["compile-js.hxml"]);
				runCommand("node", ["-e", "var unit = require('./unit.js').unit; unit.Test.main(); process.exit(unit.Test.success ? 0 : 1);"]);

				Sys.println("Test optimization:");
				Sys.setCwd(optDir);
				runCommand("haxe", ["run.hxml"]);
			case "java":
				runCommand("haxelib", ["git", "hxjava", "https://github.com/HaxeFoundation/hxjava.git"]);
				runCommand("haxe", ["compile-java.hxml"]);
				runCommand("java", ["-jar", "java/java.jar"]);
			case "cs":
				runCommand("sudo", ["apt-get", "install", "mono-devel", "mono-mcs", "-y"]);
				runCommand("haxelib", ["git", "hxcs", "https://github.com/HaxeFoundation/hxcs.git"]);
				
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
				runCommand("wget", ["http://mirror.cc.columbia.edu/pub/software/apache/flex/4.11.0/binaries/apache-flex-sdk-4.11.0-bin.tar.gz"]);
				runCommand("tar", ["-xf", "apache-flex-sdk-4.11.0-bin.tar.gz", "-C", Sys.getEnv("HOME")]);
				var flexsdkPath = Sys.getEnv("HOME") + "/apache-flex-sdk-4.11.0-bin";
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ":" + flexsdkPath + "/bin");
				var playerglobalswcFolder = flexsdkPath + "/player";
				FileSystem.createDirectory(playerglobalswcFolder + "/11.1");
				runCommand("wget", ["-nv", "http://download.macromedia.com/get/flashplayer/updaters/11/playerglobal11_1.swc", "-O", playerglobalswcFolder + "/11.1/playerglobal.swc"]);
				File.saveContent(flexsdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerglobalswcFolder');
				runCommand("mxmlc", ["--version"]);

				runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"]);
				runFlash("unit9_as3.swf");
			case target:
				throw "unknown target: " + target;
		}
	}
}