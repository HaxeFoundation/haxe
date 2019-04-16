package runci.targets;

import sys.io.File;
import sys.FileSystem;
import haxe.io.Path;
import sys.io.Process;

import runci.System.*;
import runci.Config.*;

class Flash {
	static public function getLatestFPVersion():Array<Int> {
		var appcast = Xml.parse(haxe.Http.requestUrl("http://fpdownload2.macromedia.com/get/flashplayer/update/current/xml/version_en_mac_pep.xml"));
		var versionStr = new haxe.xml.Access(appcast).node.XML.node.update.att.version;
		return versionStr.split(",").map(Std.parseInt);
	}

	static public function setupFlashPlayerDebugger():Void {
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
				var playerCmd = "flashplayerdebugger";
				if(Sys.command("type", [playerCmd]) != 0) {
					Linux.requireAptPackages([
						"libglib2.0", "libfreetype6"
					]);
					var majorVersion = getLatestFPVersion()[0];
					runCommand("wget", ["-nv", 'http://fpdownload.macromedia.com/pub/flashplayer/updaters/${majorVersion}/flash_player_sa_linux_debug.x86_64.tar.gz'], true);
					runCommand("tar", ["-xf", "flash_player_sa_linux_debug.x86_64.tar.gz", "-C", Sys.getEnv("HOME")]);
					playerCmd = Sys.getEnv("HOME") + "/flashplayerdebugger";
				}
				if (!FileSystem.exists(mmcfgPath)) {
					File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				}
				runCommand(playerCmd, ["-v"]);
			case "Mac":
				if (commandResult("brew", ["cask", "list", "flash-player-debugger"]).exitCode == 0) {
					return;
				}
				runCommand("brew", ["cask", "install", "flash-player-debugger"]);

				// Disable the "application downloaded from Internet" warning
				runCommand("xattr", ["-d", "-r", "com.apple.quarantine", "/Applications/Flash Player Debugger.app"]);

				var dir = Path.directory(mmcfgPath);
				if (!FileSystem.exists(dir)) {
					runCommand("sudo", ["mkdir", "-p", dir]);
					runCommand("sudo", ["chmod", "a+w", dir]);
				}
				if (!FileSystem.exists(mmcfgPath)) {
					File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				}
		}
	}

	/**
		Run a Flash swf file.
		Return whether the test is successful or not.
		It detemines the test result by reading the flashlog.txt, looking for "SUCCESS: true".
	*/
	static public function runFlash(swf:String):Bool {
		swf = FileSystem.fullPath(swf);
		Sys.println('going to run $swf');
		switch (systemName) {
			case "Linux":
				var playerCmd = "flashplayerdebugger";
				if(Sys.command("type", [playerCmd]) != 0) {
					playerCmd = Sys.getEnv("HOME") + "/flashplayerdebugger";
				}
				new Process(playerCmd, [swf]);
			case "Mac":
				Sys.command("open", ["-a", "/Applications/Flash Player Debugger.app", swf]);
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
			return false;
		}

		//read flashlog.txt continously
		var traceProcess = new Process("tail", ["-f", flashlogPath]);
		var success = false;
		while (true) {
			try {
				var line = traceProcess.stdout.readLine();
				if (line.indexOf("success: ") >= 0) {
					success = line.indexOf("success: true") >= 0;
					break;
				}
			} catch (e:haxe.io.Eof) {
				break;
			}
		}
		Sys.command("cat", [flashlogPath]);
		return success;
	}

	static public function run(args:Array<String>) {
		setupFlashPlayerDebugger();
		runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
		var success = runFlash("bin/unit9.swf");
		if (!success)
			fail();
	}


}