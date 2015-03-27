package io;

import sys.io.Process;

class TestProcess extends haxe.unit.TestCase {
	#if !php //FIXME
	function testArguments() {
		var bin = sys.FileSystem.absolutePath(TestArguments.bin);
		var args = TestArguments.expectedArgs;

		var process = new Process("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
		var exitCode = process.exitCode();
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);

		var process =
			#if (macro || interp)
				new Process("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
			#elseif cpp
				new Process(bin, args);
			#elseif cs
				switch (Sys.systemName()) {
					case "Windows":
						new Process(bin, args);
					case _:
						new Process("mono", [bin].concat(args));
				}
			#elseif java
				new Process("java", ["-jar", bin].concat(args));
			#elseif python
				new Process("python3", [bin].concat(args));
			#elseif neko
				new Process("neko", [bin].concat(args));
			#elseif php
				new Process("php", [bin].concat(args));
			#else
				null;
			#end
		var exitCode = process.exitCode();
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);
	}

	#if !(neko || cpp || cs) //FIXME
	function testCommandName() {
		// This is just a script that behaves like ExitCode.hx, 
		// which exits with the code same as the first given argument. 
		var scriptContent = switch (Sys.systemName()) {
			case "Windows":
				'@echo off\nexit /b %1';
			case "Mac", "Linux", _:
				'#!/bin/sh\nexit $1';
		}
		for (name in FileNames.names) {
			//call with ext
			var scriptExt = switch (Sys.systemName()) {
				case "Windows":
					".bat";
				case "Mac", "Linux", _:
					".sh";
			}
			var path = sys.FileSystem.absolutePath("temp/" + name + scriptExt);
			sys.io.File.saveContent(path, scriptContent);

			switch (Sys.systemName()) {
				case "Mac", "Linux":
					var exitCode = Sys.command("chmod", ["a+x", path]);
					assertEquals(0, exitCode);
				case "Windows":
					//pass
			}

			var random = Std.random(256);
			var exitCode = new Process(path, [Std.string(random)]).exitCode();
			if (exitCode != random)
				trace(name);
			assertEquals(random, exitCode);
			sys.FileSystem.deleteFile(path);



			//call without ext
			var scriptExt = switch (Sys.systemName()) {
				case "Windows":
					".bat";
				case "Mac", "Linux", _:
					"";
			}
			var path = sys.FileSystem.absolutePath("temp/" + name + scriptExt);
			sys.io.File.saveContent(path, scriptContent);

			switch (Sys.systemName()) {
				case "Mac", "Linux":
					var exitCode = Sys.command("chmod", ["a+x", path]);
					assertEquals(0, exitCode);
				case "Windows":
					//pass
			}

			var random = Std.random(256);
			var exitCode = new Process(path, [Std.string(random)]).exitCode();
			if (exitCode != random)
				trace(name);
			assertEquals(random, exitCode);
			sys.FileSystem.deleteFile(path);
		}
	}
	#end //!neko

	#end //!php

	function testExitCode() {
		var bin = sys.FileSystem.absolutePath(ExitCode.bin);

		// Just test only a few to save time.
		// They have special meanings: http://tldp.org/LDP/abs/html/exitcodes.html
		var codes = [0, 1, 2, 126, 127, 128, 130, 255];

		for (code in codes) {
			var args = [Std.string(code)];
			var process = new Process("haxe", ["compile-each.hxml", "--run", "ExitCode"].concat(args));
			assertEquals(code, process.exitCode());
		}

		for (code in codes) {
			var args = [Std.string(code)];
			var process =
				#if (macro || interp)
					new Process("haxe", ["compile-each.hxml", "--run", "ExitCode"].concat(args));
				#elseif cpp
					new Process(bin, args);
				#elseif cs
					switch (Sys.systemName()) {
						case "Windows":
							new Process(bin, args);
						case _:
							new Process("mono", [bin].concat(args));
					}
				#elseif java
					new Process("java", ["-jar", bin].concat(args));
				#elseif python
					new Process("python3", [bin].concat(args));
				#elseif neko
					new Process("neko", [bin].concat(args));
				#elseif php
					new Process("php", [bin].concat(args));
				#else
					null;
				#end
			assertEquals(code, process.exitCode());
		}
	}
}