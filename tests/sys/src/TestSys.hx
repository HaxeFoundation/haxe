class TestSys extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		return Sys.command(cmd, args);
	}

	function testEnv() {
		#if !(java || php || lua)
		Sys.putEnv("foo", "value");
		assertEquals("value", Sys.getEnv("foo"));
		#end
		assertEquals(null, Sys.getEnv("doesn't exist"));

		#if !(java || php || lua)
		var env = Sys.environment();
		assertEquals("value", env.get("foo"));
		#end
	}

	function testProgramPath() {
		var p = Sys.programPath();

		assertTrue(haxe.io.Path.isAbsolute(p));
		assertTrue(sys.FileSystem.exists(p));

		#if interp
			assertTrue(StringTools.endsWith(p, "Main.hx"));
		#elseif neko
			assertTrue(StringTools.endsWith(p, "sys.n"));
		#elseif cpp
			switch (Sys.systemName()) {
				case "Windows":
					assertTrue(StringTools.endsWith(p, "Main-debug.exe"));
				case _:
					assertTrue(StringTools.endsWith(p, "Main-debug"));
			}
		#elseif cs
			assertTrue(StringTools.endsWith(p, "Main-Debug.exe"));
		#elseif java
			assertTrue(StringTools.endsWith(p, "Main-Debug.jar"));
		#elseif python
			assertTrue(StringTools.endsWith(p, "sys.py"));
		#elseif php
			assertTrue(StringTools.endsWith(p, "index.php"));
		#elseif lua
			assertTrue(StringTools.endsWith(p, "sys.lua"));
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
