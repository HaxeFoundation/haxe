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
	static var playerLocation:String;

	static final DEFAULT_APACHE_MIRROR = "https://downloads.apache.org/";

	static function getPreferredApacheMirror() {
		return try {
			Json.parse(Http.requestUrl("https://www.apache.org/dyn/closer.lua?as_json=1")).preferred;
		} catch (e) {
			failMsg('Unable to determine preferred Apache mirror. Defaulting to $DEFAULT_APACHE_MIRROR');
			DEFAULT_APACHE_MIRROR;
		};
	}

	static function downloadAndExtractFlexSdk(version:String, sdkPath:String):Void {
		final apacheMirror = getPreferredApacheMirror();
		final archiveExtension = if (systemName == "Windows") "zip" else "tar.gz";
		final archiveName = 'apache-flex-sdk-${version}-bin.$archiveExtension';
		runNetworkCommand("wget", ["-nv", '${apacheMirror}/flex/${version}/binaries/$archiveName', "-O", getDownloadPath() + '/$archiveName']);

		if (systemName == "Windows") {
			runCommand("7z", ["x", getDownloadPath() + '/$archiveName', "-o" + sdkPath, "-y"]);
		} else {
			runCommand("tar", ["-xf", getDownloadPath() + '/$archiveName', "-C", getInstallPath()]);
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
			downloadAndExtractFlexSdk(flexVersion, flexSdkPath);
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

	static function getLatestFPVersion():Array<Int> {
		final appcast = Xml.parse(haxe.Http.requestUrl("http://fpdownload.macromedia.com/get/flashplayer/update/current/xml/version_en_mac_pep.xml"));
		final versionStr = new haxe.xml.Access(appcast).node.XML.node.update.att.version;
		return versionStr.split(",").map(Std.parseInt);
	}

	static function downloadPlayer(version:Int, fileName:String, outputPath:String) {
		runNetworkCommand("wget", [
			"-nv",
			'https://fpdownload.macromedia.com/pub/flashplayer/updaters/$version/$fileName',
			"-O",
			'$outputPath/$fileName'
		]);
	}

	static function setupFlashPlayerMac():Void {
		playerLocation = getInstallPath() + "/Flash Player.app";

		if (FileSystem.exists(playerLocation))
			infoMsg('Flash player found at $playerLocation');
		else {
			final majorVersion = getLatestFPVersion()[0];
			final packageName = 'flashplayer_${majorVersion}_sa_debug.dmg';
			downloadPlayer(majorVersion, packageName, getDownloadPath());
			runCommand("hdiutil", ["attach", getDownloadPath() + '/$packageName']);

			runCommand("cp", ["-R", "/Volumes/Flash Player/Flash Player.app", getInstallPath()]);
			runCommand("hdiutil", ["detach", "/Volumes/Flash Player"]);

			// Disable the "application downloaded from Internet" warning
			runCommand("xattr", ["-d", "-r", "com.apple.quarantine", playerLocation]);
		}

		runCommand("open", ["-a", playerLocation, "-v"]);
	}

	static function setupFlashPlayerLinux():Void {
		final playerName = "flashplayerdebugger";
		playerLocation = Path.join([getInstallPath(), playerName]);
		if (Sys.command("type", [playerName]) == 0) {
			playerLocation = playerName;
			infoMsg('Using $playerName from PATH');
		} else if(FileSystem.exists(playerLocation)) {
			infoMsg('Flash player found at $playerLocation');
		} else {
			Linux.requireAptPackages(["libglib2.0-0", "libfreetype6"]);
			final tarFileName = 'flash_player_sa_linux_debug.x86_64.tar.gz';
			downloadPlayer(getLatestFPVersion()[0], tarFileName, getDownloadPath());
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

	static function setupFlashPlayerWindows():Void {
		final majorVersion = getLatestFPVersion()[0];
		final exeName = 'flashplayer_${majorVersion}_sa_debug.exe';
		playerLocation = Path.join([getInstallPath(), exeName]);

		if (FileSystem.exists(playerLocation))
			infoMsg('Flash player found at $playerLocation');
		else
			downloadPlayer(majorVersion, exeName, getInstallPath());
	}

	static function setupFlashPlayer():Void {
		switch (systemName) {
			case "Linux":
				setupFlashPlayerLinux();
			case "Mac":
				setupFlashPlayerMac();
			case "Windows":
				setupFlashPlayerWindows();
			case _:
				throw "unsupported system";
		}
	}

	static function readUntil(stdout:haxe.io.Input, expected:String, ?unexpectedStrings:Map<String, ()->Void>) {
		final possibleStrings = unexpectedStrings?.copy() ?? [];
		possibleStrings[expected] = function() {};
		var output = "";
		while (true) {
			final char = try {
				String.fromCharCode(stdout.readByte());
			} catch (e:haxe.io.Eof) {
				failMsg('Expected to find "$expected" in stdout');
				throw new CommandFailure();
			};
			Sys.print(char);
			output += char;
			for (string => onMatch in possibleStrings) {
				if (output.endsWith(string)) {
					onMatch();
					return;
				}
			}
		}
	}

	/**
		Run a Flash swf file.
		Throws `CommandFailure` if unsuccessful.
	*/
	static function runFlash(swf:String):Void {
		swf = FileSystem.fullPath(swf);
		infoMsg('Running .swf file: $swf');

		final debuggerProcess = new Process("fdb");

		final FDB_PROMPT = "(fdb) ";
		// waits for the fdb prompt and runs a command
		function runDebuggerCommand(command:String) {
			readUntil(debuggerProcess.stdout, FDB_PROMPT);
			Sys.println(command);
			debuggerProcess.stdin.writeString('$command\n');
		}

		runDebuggerCommand("run");

		function onUnexpectedPrompt() {
			Sys.println("quit");
			debuggerProcess.stdin.writeString("quit\n");
			throw new CommandFailure();
		}

		readUntil(debuggerProcess.stdout, "Waiting for Player to connect", [FDB_PROMPT => onUnexpectedPrompt]);
		final playerProcess = switch (systemName) {
			case "Linux" if (ci == GithubActions):
				new Process("xvfb-run", ["-a", playerLocation, swf]);
			case "Mac":
				new Process("open", ["-a", playerLocation, swf]);
			default:
				new Process(playerLocation, [swf]);
		};

		readUntil(debuggerProcess.stdout, "Player connected; session starting.", [FDB_PROMPT => onUnexpectedPrompt]);
		runDebuggerCommand("continue");

		var success = true;
		readUntil(debuggerProcess.stdout, "(success: true)", [
			FDB_PROMPT => onUnexpectedPrompt,
			"(success: false)" => () -> { success = false; }
		]);

		runDebuggerCommand("quit");

		debuggerProcess.kill();
		debuggerProcess.close();
		playerProcess.kill();
		playerProcess.close();

		if (!success)
			throw new CommandFailure();
	}

	static public function run(args:Array<String>) {
		setupFlashPlayer();
		setupFlexSdk();
		for (flashVersion in ["11", "32"]) {
			runCommand("haxe", ["compile-flash.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids", "--swf-version", flashVersion].concat(args));
			runFlash("bin/unit.swf");
		}

		changeDirectory(miscFlashDir);
		runCommand("haxe", ["run.hxml"]);
	}

}
