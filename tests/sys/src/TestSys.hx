class TestSys extends haxe.unit.TestCase {
	#if !php //https://github.com/HaxeFoundation/haxe/issues/3603#issuecomment-86437474
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
