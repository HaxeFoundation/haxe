import sys.*;
import sys.io.*;

class RunTravis {
	static function runProcess(cmd:String, args:Array<String>):Void {
		var p = new Process(cmd, args);
		Sys.println(p.stdout.readAll().toString());
		Sys.println(p.stderr.readAll().toString());

		var exitCode = p.exitCode();
		Sys.println('Process exited with $exitCode: $cmd $args');

		if (exitCode != 0) {
			Sys.exit(1);
		}
	}

	static function setupFlashPlayerDebugger():Void {
		Sys.putEnv("DISPLAY", ":99.0");
		runProcess("sh", ["-e", "/etc/init.d/xvfb", "start"]);
		Sys.putEnv("AUDIODEV", "null");
		runProcess("sudo", ["apt-get", "install", "-qq", "libgd2-xpm", "ia32-libs", "ia32-libs-multiarch", "flashplugin-installer", "-y"]);
		runProcess("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/11/flashplayer_11_sa_debug.i386.tar.gz"]);
		runProcess("tar", ["-xvf", "flashplayer_11_sa_debug.i386.tar.gz"]);
		File.saveContent(Sys.getEnv("HOME") + "/mm.cfg", "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
		runProcess("./flashplayerdebugger", ["-v"]);
	}

	static function runFlash(flashplayerdebuggerProcess:Process):Void {
		//wait a little until flashlog.txt is created
		var flashlogPath = Sys.getEnv("HOME") + "/.macromedia/Flash_Player/Logs/flashlog.txt";
		for (t in 0...5) {
			if (FileSystem.exists(flashlogPath))
				break;
			else
				runProcess("sleep", ["2"]); 
		}				
		if (!FileSystem.exists(flashlogPath)) {
			//the flashplayerdebugger should has already exited with some error...
			Sys.println(flashplayerdebuggerProcess.stdout.readAll().toString());
			Sys.println(flashplayerdebuggerProcess.stderr.readAll().toString());

			var exitCode = flashplayerdebuggerProcess.exitCode();
			Sys.println('flashplayerdebuggerProcess exited with $exitCode');
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
		switch (Sys.getEnv("TARGET")) {
			case "macro":
				runProcess("haxe", ["compile-macro.hxml"]);
			case "neko":
				runProcess("haxe", ["compile-neko.hxml"]);
				runProcess("neko", ["unit.n"]);
			case "php":
				runProcess("sudo", ["apt-get", "install", "php5", "-y"]);
				runProcess("haxe", ["compile-php.hxml"]);
				runProcess("php", ["php/index.php"]);
			case "cpp":
				//hxcpp dependencies
				runProcess("sudo", ["apt-get", "install", "gcc-multilib", "g++-multilib", "-y"]);

				//install and build hxcpp
				runProcess("haxelib", ["git", "hxcpp", "https://github.com/HaxeFoundation/hxcpp.git"]);
				Sys.setCwd(Sys.getEnv("HOME") + "/haxelib/hxcpp/git/runtime/");
				runProcess("haxelib", ["run", "hxcpp", "BuildLibs.xml"]);
				Sys.setCwd(cwd);
				
				runProcess("haxe", ["compile-cpp.hxml"]);
				runProcess("./cpp/Test-debug", []);
			case "js":
				runProcess("haxe", ["compile-js.hxml"]);
				runProcess("node", ["-e", "var unit = require('./unit.js').unit; unit.Test.main(); process.exit(unit.Test.success ? 0 : 1);"]);
			case "java":
				runProcess("haxelib", ["git", "hxjava", "https://github.com/HaxeFoundation/hxjava.git"]);
				runProcess("haxe", ["compile-java.hxml"]);
				runProcess("java", ["-jar", "java/java.jar"]);
			case "cs":
				runProcess("sudo", ["apt-get", "install", "mono-devel", "mono-mcs", "-y"]);
				runProcess("haxelib", ["git", "hxcs", "https://github.com/HaxeFoundation/hxcs.git"]);
				
				runProcess("haxe", ["compile-cs.hxml"]);
				runProcess("mono", ["cs/bin/Test-Debug.exe"]);

				runProcess("haxe", ["compile-cs-unsafe.hxml"]);
				runProcess("mono", ["cs_unsafe/bin/Test-Debug.exe"]);
			case "flash9":
				setupFlashPlayerDebugger();
				runProcess("haxe", ["compile-flash9.hxml", "-D", "fdb"]);
				runFlash(new Process("./flashplayerdebugger", ["unit9.swf"]));
			case "flash8":
				setupFlashPlayerDebugger();
				runProcess("haxe", ["compile-flash8.hxml", "-D", "fdb"]);
				runFlash(new Process("./flashplayerdebugger", ["unit8.swf"]));
			// case "as3":
			// 	//install Apache Flex
			// 	//see https://cwiki.apache.org/confluence/display/FLEX/1.3+Setting+up+Linux+(if+having+trouble)
			// 	runProcess("sudo", ["apt-get", "install", "ia32-libs", "-y"]); //AIR is 32-bit only
			// 	runProcess("wget", ["http://update.devolo.com/linux/apt/pool/main/a/adobeair/adobeair_2.6.0.19170_amd64.deb"]);
			// 	runProcess("sudo", ["dpkg", "-i", "adobeair_2.6.0.19170_amd64.deb"]);
			// 	runProcess("wget", ["http://apache.communilink.net/flex/installer/2.7/binaries/apache-flex-sdk-installer-2.7.0-bin.deb"]);
			// 	runProcess("sudo", ["dpkg", "-i", "--force-depends", "apache-flex-sdk-installer-2.7.0-bin.deb"]);
			// 	Sys.setCwd("/opt/Apache Flex/Apache Flex SDK Installer/bin");
			// 	runProcess("./Apache Flex SDK Installer", []);
			// 	Sys.setCwd(cwd);

			// 	runProcess("sudo", ["apt-get", "install", "flashplugin-installer", "-y"]);
			case target:
				throw "unknown target: " + target;
		}
	}
}