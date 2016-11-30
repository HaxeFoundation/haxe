import sys.*;
import haxe.io.*;

class TestCommandBase extends haxe.unit.TestCase {
	var runInfo:{out:String, err:String} = null;
	function run(cmd:String, ?args:Array<String>):Int {
		throw "should be overridden";
	}

	function testCommand() {
		var bin = FileSystem.absolutePath(TestArguments.bin);
		var args = TestArguments.expectedArgs;

		#if !cs
		var exitCode = run("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);
		#end

		var exitCode =
			#if (macro || interp)
				run("haxe", ["compile-each.hxml", "--run", "TestArguments"].concat(args));
			#elseif cpp
				run(bin, args);
			#elseif cs
				switch (Sys.systemName()) {
					case "Windows":
						run(bin, args);
					case _:
						run("mono", [bin].concat(args));
				}
			#elseif java
				run(Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", bin].concat(args));
			#elseif python
				run(python.lib.Sys.executable, [bin].concat(args));
			#elseif neko
				run("neko", [bin].concat(args));
			#elseif php
				run(untyped __php__("defined('PHP_BINARY') ? PHP_BINARY : 'php'"), [bin].concat(args));
			#elseif lua
				switch(Sys.getEnv("LUA")){
					case null : run("lua", [bin].concat(args));
					default   : run(Sys.getEnv("LUA"), [bin].concat(args));
				};
			#else
				-1;
			#end
		if (exitCode != 0)
			trace(sys.io.File.getContent(TestArguments.log));
		assertEquals(0, exitCode);
	}

	function testCommandName() {
		var binExt = switch (Sys.systemName()) {
			case "Windows":
				".exe";
			case "Mac", "Linux", _:
				"";
		}

		for (name in FileNames.names) {
			if ((name + binExt).length < 256) {
				var path = FileSystem.absolutePath("temp/" + name + binExt);
				switch (Sys.systemName()) {
					case "Windows":
						sys.io.File.copy(ExitCode.getNative(), path);
					case "Mac", "Linux", _:
						var exitCode = run("cp", [ExitCode.getNative(), path]);
						assertEquals(0, exitCode);
				}

				Sys.sleep(0.1);

				var random = Std.random(256);
				var exitCode = try {
					run(path, [Std.string(random)]);
				} catch (e:Dynamic) {
					trace(e);
					trace(name);
					throw e;
				}
				if (exitCode != random)
					trace(name);
				assertEquals(random, exitCode);
				FileSystem.deleteFile(path);
			}
		}
	}

	function testExitCode() {
		var bin = FileSystem.absolutePath(ExitCode.bin);

		// Just test only a few to save time.
		// They have special meanings: http://tldp.org/LDP/abs/html/exitcodes.html
		var codes = [0, 1, 2, 126, 127, 128, 130, 255];

		for (code in codes) {
			var args = [Std.string(code)];
			var exitCode = run(ExitCode.getNative(), args);
			assertEquals(code, exitCode);
		}

		for (code in codes) {
			var args = [Std.string(code)];
			var exitCode =
				#if (macro || interp)
					run("haxe", ["compile-each.hxml", "--run", "ExitCode"].concat(args));
				#elseif cpp
					run(bin, args);
				#elseif cs
					switch (Sys.systemName()) {
						case "Windows":
							run(bin, args);
						case _:
							run("mono", [bin].concat(args));
					}
				#elseif java
					run(Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", bin].concat(args));
				#elseif python
					run(python.lib.Sys.executable, [bin].concat(args));
				#elseif neko
					run("neko", [bin].concat(args));
				#elseif php
					run(untyped __php__("defined('PHP_BINARY') ? PHP_BINARY : 'php'"), [bin].concat(args));
				#elseif lua
					switch(Sys.getEnv("LUA")){
						case null: run("lua", [bin].concat(args));
						default : run(Sys.getEnv("LUA"), [bin].concat(args));
					};
				#else
					-1;
				#end
			if ((code != exitCode) && (runInfo != null)) {
				trace(runInfo);
			}
			assertEquals(code, exitCode);
		}
	}

	function testRawCommand() {
		var bin = sys.FileSystem.absolutePath(ExitCode.bin);
		var native = sys.FileSystem.absolutePath(ExitCode.getNative());
		var exitCode = run('$native 1 || $native 0');
		assertEquals(0, exitCode);
	}
}
