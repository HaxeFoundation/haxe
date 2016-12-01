import sys.*;
import sys.io.*;
import haxe.io.*;

/**
	This is intented to be used by TestSys and io.TestProcess.
*/
class ExitCode {
	static public var bin:String =
	#if neko
		"bin/neko/ExitCode.n";
	#elseif cpp
		#if debug
			"bin/cpp/ExitCode-debug";
		#else
			"bin/cpp/ExitCode";
		#end
	#elseif cs
		#if debug
			"bin/cs/bin/ExitCode-Debug.exe";
		#else
			"bin/cs/bin/ExitCode.exe";
		#end
	#elseif java
		#if debug
			"bin/java/ExitCode-Debug.jar";
		#else
			"bin/java/ExitCode.jar";
		#end
	#elseif python
		"bin/python/ExitCode.py";
	#elseif php7
		"bin/php7/ExitCode/index.php";
	#elseif php
		"bin/php/ExitCode/index.php";
	#elseif lua
		"bin/lua/ExitCode.lua";
	#else
		null;
	#end

	static public function getNative():String {
		// This is just a script that behaves like ExitCode.hx,
		// which exits with the code same as the first given argument.
		// var scriptContent = switch (Sys.systemName()) {
		// 	case "Windows":
		// 		'@echo off\nexit /b %1';
		// 	case "Mac", "Linux", _:
		// 		'#!/bin/sh\nexit $1';
		// }
		// var scriptExt = switch (Sys.systemName()) {
		// 	case "Windows":
		// 		".bat";
		// 	case "Mac", "Linux", _:
		// 		".sh";
		// }

		var binExt = switch (Sys.systemName()) {
			case "Windows":
				".exe";
			case "Mac", "Linux", _:
				"";
		}

		var binPath = Path.join(["bin", "ExitCode"]) + binExt;
		if (FileSystem.exists(binPath)) {
			return binPath;
		}

		if (!FileSystem.exists("bin"))
			FileSystem.createDirectory("bin");

		switch (Sys.systemName()) {
			case "Windows":
				// var gcc = Sys.command("cl", ["src/ExitCode.c", "/Fobin", "/link", "/out:bin/ExitCode.exe"]);
				// if (gcc != 0)
				// 	throw "cannot compile ExitCode";
				File.copy(Path.join(["bin", "neko", "ExitCode"]) + binExt, binPath);
			case "Mac", "Linux", _:
				var gcc = Sys.command("gcc", ["src/ExitCode.c", "-o", "bin/ExitCode"]);
				if (gcc != 0)
					throw "cannot compile ExitCode";
		}

		return binPath;
	}

	static function main():Void {
		Sys.exit(Std.parseInt(Sys.args()[0]));
	}
}
