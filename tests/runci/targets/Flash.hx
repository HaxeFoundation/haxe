package runci.targets;

import sys.FileSystem;
import sys.io.File;
import sys.io.Process;
import haxe.Json;
import haxe.Http;
import haxe.io.Path;

import runci.System.*;
import runci.System.CommandFailure;
import runci.Config.*;

using StringTools;

class Flash {
	static final miscFlashDir = getMiscSubDir('flash');

	static function getLatestFPVersion():Array<Int> {
		final appcast = Xml.parse(haxe.Http.requestUrl("http://fpdownload.macromedia.com/get/flashplayer/update/current/xml/version_en_mac_pep.xml"));
		final versionStr = new haxe.xml.Access(appcast).node.XML.node.update.att.version;
		return versionStr.split(",").map(Std.parseInt);
	}

	static final defaultApacheMirror = "https://downloads.apache.org/";

	static function getPreferredApacheMirror() {
		return try {
			Json.parse(Http.requestUrl("https://www.apache.org/dyn/closer.lua?as_json=1")).preferred;
		} catch (e) {
			failMsg('Unable to determine preferred Apache mirror. Defaulting to $defaultApacheMirror');
			defaultApacheMirror;
		}
	}

	static function downloadFlexSdk(version:String, sdkPath:String):Void {
		final apacheMirror = getPreferredApacheMirror();
		if (systemName == "Windows") {
			final zipName = 'apache-flex-sdk-${version}-bin.zip';
			runNetworkCommand("wget", ["-nv", '${apacheMirror}/flex/${version}/binaries/$zipName', "-O", getDownloadPath() + '/$zipName']);
			runCommand("7z", ["x", getDownloadPath() + '/$zipName', "-o" + sdkPath, "-y"]);
		} else {
			final tarName = 'apache-flex-sdk-${version}-bin.tar.gz';
			runNetworkCommand("wget", ["-nv", '${apacheMirror}/flex/${version}/binaries/$tarName', "-O", getDownloadPath() + '/$tarName']);
			runCommand("tar", ["-xf", getDownloadPath() + '/$tarName', "-C", getInstallPath()]);
		}
	}

	static function setupFlexSdk():Void {
		if (commandSucceed("mxmlc", ["--version"])) {
			infoMsg('mxmlc has already been installed.');
			return;
		}
		// download flex sdk
		final flexVersion = "4.16.0";
		final flexSdkPath = Path.normalize(getInstallPath() + '/apache-flex-sdk-${flexVersion}-bin');

		if (FileSystem.exists(flexSdkPath)) {
			infoMsg('Flex SDK found at $flexSdkPath');
		} else {
			downloadFlexSdk(flexVersion, flexSdkPath);
		}
		addToPATH(flexSdkPath + "/bin");

		// download playerglobal.swc
		final playerGlobalSwcFolder = flexSdkPath + "/player";
		FileSystem.createDirectory(playerGlobalSwcFolder + "/11.1");
		final flashVersion = getLatestFPVersion();

		final playerGlobalSwcPath = playerGlobalSwcFolder + "/11.1/playerglobal.swc";

		if (FileSystem.exists(playerGlobalSwcPath)) {
			infoMsg('playerglobal.swc found at $playerGlobalSwcPath');
		} else {
			runNetworkCommand("wget", [
				"-nv",
				'https://fpdownload.macromedia.com/get/flashplayer/updaters/${flashVersion[0]}/playerglobal${flashVersion[0]}_${flashVersion[1]}.swc',
				"-O",
				playerGlobalSwcPath
			]);

		}
		// set playerglobal.swc
		File.saveContent(flexSdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerGlobalSwcFolder');

		// ensure flex sdk is working
		runCommand("mxmlc", ["--version"]);
	}

	static var playerLocation:String;
	static var flashlogPath:String;

	static function createConfigFile(location:String):Void {
		final mmcfgPath = location + "/mm.cfg";
		if (!FileSystem.exists(mmcfgPath))
			File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
	}

	static function setupFlashDebuggerMac():Void {
		playerLocation = getInstallPath() + "/Flash Player.app";
		flashlogPath = Sys.getEnv("HOME") + "/Library/Preferences/Macromedia/Flash Player/Logs/flashlog.txt";

		if (FileSystem.exists(playerLocation))
			infoMsg('Flash player found at $playerLocation');
		else {
			final majorVersion = getLatestFPVersion()[0];
			final packageName = "flashplayer_32_sa_debug.dmg";
			runNetworkCommand("wget", [
				"-nv",
				'https://fpdownload.macromedia.com/pub/flashplayer/updaters/${majorVersion}/$packageName',
				"-O",
				getDownloadPath() + '/$packageName'
			]);
			runCommand("hdiutil", ["attach", getDownloadPath() + '/$packageName']);

			runCommand("cp", ["-R", "/Volumes/Flash Player/Flash Player.app", getInstallPath()]);
			runCommand("hdiutil", ["detach", "/Volumes/Flash Player"]);

			// Disable the "application downloaded from Internet" warning
			runCommand("xattr", ["-d", "-r", "com.apple.quarantine", playerLocation]);
		}

		final configLocation = "/Library/Application Support/Macromedia";
		if (!FileSystem.exists(configLocation)) {
			runCommand("sudo", ["mkdir", "-p", configLocation]);
			runCommand("sudo", ["chmod", "a+w", configLocation]);
		}
		createConfigFile(configLocation);

		runCommand("open", ["-a", playerLocation, "-v"]);
	}

	static function setupFlashDebuggerLinux():Void {
		flashlogPath = Sys.getEnv("HOME") + "/.macromedia/Flash_Player/Logs/flashlog.txt";

		final debuggerName = "flashplayerdebugger";
		playerLocation = Path.join([getInstallPath(), debuggerName]);
		if (Sys.command("type", [debuggerName]) == 0) {
			playerLocation = debuggerName;
			infoMsg('Using $debuggerName from PATH');
		} else if(FileSystem.exists(playerLocation)) {
			infoMsg('Flash player found at $playerLocation');
		} else {
			Linux.requireAptPackages(["libglib2.0-0", "libfreetype6"]);
			final majorVersion = getLatestFPVersion()[0];
			final tarFileName = 'flash_player_sa_linux_debug.x86_64.tar.gz';
			runNetworkCommand("wget", [
				"-nv",
				'https://fpdownload.macromedia.com/pub/flashplayer/updaters/${majorVersion}/$tarFileName',
				"-O",
				getDownloadPath() + '/$tarFileName'
			]);
			runCommand("tar", ["-xf", getDownloadPath() + '/$tarFileName', "-C", getInstallPath()]);
		}

		createConfigFile(Sys.getEnv("HOME"));

		// ensure the debugger works
		switch (ci) {
			case GithubActions:
				runCommand("xvfb-run", ["-a", playerLocation, "-v"]);
			case _:
				runCommand(playerLocation, ["-v"]);
		}
	}

	static function setupFlashDebuggerWindows():Void {
		flashlogPath = Sys.getEnv("APPDATA") + "\\Macromedia\\Flash Player\\Logs\\flashlog.txt";
		final exeName = "flashplayer_32_sa_debug.exe";
		playerLocation = Path.join([getInstallPath(), exeName]);

		final majorVersion = getLatestFPVersion()[0];
		if (FileSystem.exists(playerLocation))
			infoMsg('Flash player found at $playerLocation');
		else
			runNetworkCommand("wget", [
				"-nv",
				'https://fpdownload.macromedia.com/pub/flashplayer/updaters/${majorVersion}/$exeName',
				"-O",
				playerLocation
			]);

		createConfigFile(Sys.getEnv("HOMEDRIVE") + "\\" + Sys.getEnv("HOMEPATH"));
	}

	static function setupFlashPlayerDebugger():Void {
		switch (systemName) {
			case "Linux":
				setupFlashDebuggerLinux();
			case "Mac":
				setupFlashDebuggerMac();
			case "Windows":
				setupFlashDebuggerWindows();
			case _:
				throw "unsupported system";
		}
	}

	/**
		Run a Flash swf file.
		Throws `CommandFailure` if unsuccessful.
		It detemines the test result by reading the flashlog.txt, looking for "success: true".
	*/
	static function runFlash(swf:String):Void {
		swf = FileSystem.fullPath(swf);
		infoMsg('Running .swf file: $swf');

		final runProcess = switch (systemName) {
			case "Linux" if (ci == GithubActions):
				new Process("xvfb-run", ["-a", playerLocation, swf]);
			case "Mac":
				new Process("open", ["-a", playerLocation, swf]);
			default:
				new Process(playerLocation, [swf]);
		}

		// wait a little until flashlog.txt is created
		for (_ in 0...5) {
			infoMsg("Waiting 2 seconds for flash log file...");
			Sys.sleep(2);
			if (FileSystem.exists(flashlogPath))
				break;
		}
		if (!FileSystem.exists(flashlogPath)) {
			failMsg('$flashlogPath not found.');
			throw new CommandFailure();
		}

		// read flashlog.txt continously
		final traceProcess = switch (systemName) {
			case "Windows":
				new Process("powershell", ["-command", '& {Get-Content "$flashlogPath" -Wait -Tail 1}']);
			default:
				new Process("tail", ["-f", flashlogPath]);
		}
		var success = false;
		while (true) {
			try {
				final line = traceProcess.stdout.readLine();
				if (line.indexOf("success: ") >= 0) {
					success = line.indexOf("success: true") >= 0;
					break;
				}
			} catch (e:haxe.io.Eof) {
				break;
			}
		}
		runProcess.kill();
		runProcess.close();

		traceProcess.kill();
		traceProcess.close();
		final cmd = (systemName == "Windows") ? "type" : "cat";
		Sys.command(cmd, [flashlogPath]);
		if (!success)
			throw new CommandFailure();
	}

	static public function run(args:Array<String>) {
		setupFlashPlayerDebugger();
		setupFlexSdk();
		for (argsVariant in [[], ["--swf-version", "32"]]) {
			runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args).concat(argsVariant));
			runFlash("bin/unit9.swf");
		}

		changeDirectory(miscFlashDir);
		runCommand("haxe", ["run.hxml"]);
	}

}
