class TestSys extends haxe.unit.TestCase {
	#if !php //FIXME https://github.com/HaxeFoundation/haxe/issues/3603#issuecomment-86437474
	function testCommand() {
		var bin = sys.FileSystem.absolutePath(TestArguments.bin);
		var args = TestArguments.expectedArgs;

		var exitCode = Sys.command("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);

		var exitCode =
			#if (macro || interp)
				Sys.command("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
			#elseif cpp
				Sys.command(bin, args);
			#elseif cs
				switch (Sys.systemName()) {
					case "Windows":
						Sys.command(bin, args);
					case _:
						Sys.command("mono", [bin].concat(args));
				}
			#elseif java
				Sys.command("java", ["-jar", bin].concat(args));
			#elseif python
				Sys.command("python3", [bin].concat(args));
			#elseif neko
				Sys.command("neko", [bin].concat(args));
			#elseif php
				Sys.command("php", [bin].concat(args));
			#else
				-1;
			#end
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);
	}

	#if !cs //FIXME
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
			var exitCode = Sys.command(path, [Std.string(random)]);
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
			var exitCode = Sys.command(path, [Std.string(random)]);
			if (exitCode != random)
				trace(name);
			assertEquals(random, exitCode);
			sys.FileSystem.deleteFile(path);
		}
	}
	#end //!cs

	function testExitCode() {
		var bin = sys.FileSystem.absolutePath(ExitCode.bin);

		// Just test only a few to save time.
		// They have special meanings: http://tldp.org/LDP/abs/html/exitcodes.html
		var codes = [0, 1, 2, 126, 127, 128, 130, 255];

		for (code in codes) {
			var args = [Std.string(code)];
			var exitCode = Sys.command("haxe", ["compile-each.hxml", "--run", "ExitCode"].concat(args));
			assertEquals(code, exitCode);
		}

		for (code in codes) {
			var args = [Std.string(code)];
			var exitCode =
				#if (macro || interp)
					Sys.command("haxe", ["compile-each.hxml", "--run", "ExitCode"].concat(args));
				#elseif cpp
					Sys.command(bin, args);
				#elseif cs
					switch (Sys.systemName()) {
						case "Windows":
							Sys.command(bin, args);
						case _:
							Sys.command("mono", [bin].concat(args));
					}
				#elseif java
					Sys.command("java", ["-jar", bin].concat(args));
				#elseif python
					Sys.command("python3", [bin].concat(args));
				#elseif neko
					Sys.command("neko", [bin].concat(args));
				#elseif php
					Sys.command("php", [bin].concat(args));
				#else
					-1;
				#end
			assertEquals(code, exitCode);
		}
	}
	#end

	function testEnv() {
		#if !(java || php)
		Sys.putEnv("foo", "value");
		assertEquals("value", Sys.getEnv("foo"));
		#end
		assertEquals(null, Sys.getEnv("doesn't exist"));

		#if !(java || php)
		var env = Sys.environment();
		assertEquals("value", env.get("foo"));
		#end
	}

	#if !java
	function testCwd() {
		var cur = Sys.getCwd();
		Sys.setCwd("../");
		var newCwd = haxe.io.Path.join([cur, "../"]);
		function normalize(path) {
			return haxe.io.Path.addTrailingSlash(haxe.io.Path.normalize(path));
		}
		assertEquals(normalize(newCwd), normalize(Sys.getCwd()));
		Sys.setCwd(cur);
	}
	#end
}
