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
		final flexVersion = "4.16.1";
		final flexSdkPath = Path.normalize(getInstallPath() + '/apache-flex-sdk-${flexVersion}-bin');

		if (FileSystem.exists(flexSdkPath)) {
			infoMsg('Flex SDK found at $flexSdkPath');
		} else {
			downloadFlexSdk(flexVersion, flexSdkPath);
		}
		addToPATH(flexSdkPath + "/bin");

		// download playerglobal.swc
		final playerGlobalSwcFolder = flexSdkPath + "/player";
		final playerGlobalSwcSubFolder = playerGlobalSwcFolder + "/27.0";
		FileSystem.createDirectory(playerGlobalSwcSubFolder);

		final playerGlobalSwcPath = '$playerGlobalSwcSubFolder/playerglobal.swc';

		if (FileSystem.exists(playerGlobalSwcPath)) {
			infoMsg('playerglobal.swc found at $playerGlobalSwcPath');
		} else {
			final flashVersion = getLatestFPVersion();
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

	static function setupFlashDebuggerMac():Void {
		playerLocation = getInstallPath() + "/Flash Player.app";

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

		runCommand("open", ["-a", playerLocation, "-v"]);
	}

	static function setupFlashDebuggerLinux():Void {
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

		// ensure the debugger works
		switch (ci) {
			case GithubActions:
				runCommand("xvfb-run", ["-a", playerLocation, "-v"]);
			case _:
				runCommand(playerLocation, ["-v"]);
		}
	}

	static function setupFlashDebuggerWindows():Void {
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

	static function readUntil(stdout:haxe.io.Input, expected:String) {
		var output = "";
		while (true) {
			output += try {
				String.fromCharCode(stdout.readByte());
			} catch (e:haxe.io.Eof) {
				failMsg('Expected to find "$expected". Output was:');
				Sys.print(output);
				throw new CommandFailure();
			};
			if (output.endsWith(expected)) {
				break;
			}
		}
		return output;
	}

	/**
		Run a Flash swf file.
		Throws `CommandFailure` if unsuccessful.
	*/
	static function runFlash(swf:String):Void {
		swf = FileSystem.fullPath(swf);
		infoMsg('Running .swf file: $swf');

		final debuggerProcess = new Process("fdb");

		var output = "";

		// waits for the fdb prompt and runs a command
		function runDebuggerCommand(command:String) {
			output += readUntil(debuggerProcess.stdout, "(fdb) ") + '$command\n';
			debuggerProcess.stdin.writeString('$command\n');
		}

		runDebuggerCommand("run");

		final playerProcess = switch (systemName) {
			case "Linux" if (ci == GithubActions):
				new Process("xvfb-run", ["-a", playerLocation, swf]);
			case "Mac":
				new Process("open", ["-a", playerLocation, swf]);
			default:
				new Process(playerLocation, [swf]);
		};

		runDebuggerCommand("continue");

		output += readUntil(debuggerProcess.stdout, "results: ");

		final results = readUntil(debuggerProcess.stdout, "\n");
		final success = results.contains("(success: true)");
		output += results;

		runDebuggerCommand("quit");

		Sys.print(output);

		debuggerProcess.kill();
		debuggerProcess.close();
		playerProcess.kill();
		playerProcess.close();

		if (!success)
			throw new CommandFailure();
	}

	static public function run(args:Array<String>) {
		setupFlashPlayerDebugger();
		setupFlexSdk();
		for (flashVersion in ["11", "32"]) {
			runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids", "--swf-version", flashVersion].concat(args));
			runFlash("bin/unit9.swf");
		}

		changeDirectory(miscFlashDir);
		runCommand("haxe", ["run.hxml"]);
	}

}
