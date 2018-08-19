package runci.targets;

import sys.io.File;
import sys.FileSystem;
import haxe.io.Path;
import sys.io.Process;

import runci.System.*;
import runci.Config.*;

class Flash {

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
				Linux.requireAptPackages([
					"libglib2.0", "libfreetype6"
				]);
				runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/30/flash_player_sa_linux_debug.x86_64.tar.gz"], true);
				runCommand("tar", ["-xf", "flash_player_sa_linux_debug.x86_64.tar.gz", "-C", Sys.getEnv("HOME")]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				runCommand(Sys.getEnv("HOME") + "/flashplayerdebugger", ["-v"]);
			case "Mac":
				runCommand("brew", ["tap", "caskroom/versions"]);
				runCommand("brew", ["cask", "install", "flash-player-debugger"]);
				var dir = Path.directory(mmcfgPath);
				runCommand("sudo", ["mkdir", "-p", dir]);
				runCommand("sudo", ["chmod", "a+w", dir]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
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
				new Process(Sys.getEnv("HOME") + "/flashplayerdebugger", [swf]);
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
		var line = "";
		while (true) {
			try {
				line = traceProcess.stdout.readLine();
				Sys.println(line);
				if (line.indexOf("SUCCESS: ") >= 0) {
					return line.indexOf("SUCCESS: true") >= 0;
				}
			} catch (e:haxe.io.Eof) {
				break;
			}
		}
		return false;
	}

	static public function run(args:Array<String>) {
		setupFlashPlayerDebugger();
		runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
		var success = runFlash("bin/unit9.swf");
		if (!success)
			fail();
	}
}