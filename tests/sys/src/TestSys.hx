import utest.Assert;

class TestSys extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		return Sys.command(cmd, args);
	}

	function testEnv() {
		#if !(java)
		Sys.putEnv("foo", "value");
		Assert.equals("value", Sys.getEnv("foo"));
		#end
		Assert.equals(null, Sys.getEnv("doesn't exist"));

		#if !(java)
		var env = Sys.environment();
		Assert.equals("value", env.get("foo"));
		#end
	}

	function testProgramPath() {
		var p = Sys.programPath();

		Assert.isTrue(haxe.io.Path.isAbsolute(p));
		Assert.isTrue(sys.FileSystem.exists(p));

		#if interp
			Assert.isTrue(StringTools.endsWith(p, "Main.hx"));
		#elseif neko
			Assert.isTrue(StringTools.endsWith(p, "sys.n"));
		#elseif cpp
			switch (Sys.systemName()) {
				case "Windows":
					Assert.isTrue(StringTools.endsWith(p, "Main-debug.exe"));
				case _:
					Assert.isTrue(StringTools.endsWith(p, "Main-debug"));
			}
		#elseif cs
			Assert.isTrue(StringTools.endsWith(p, "Main-Debug.exe"));
		#elseif jvm
			Assert.isTrue(StringTools.endsWith(p, "sys.jar"));
		#elseif java
			Assert.isTrue(StringTools.endsWith(p, "Main-Debug.jar"));
		#elseif python
			Assert.isTrue(StringTools.endsWith(p, "sys.py"));
		#elseif php
			Assert.isTrue(StringTools.endsWith(p, "index.php"));
		#elseif lua
			Assert.isTrue(StringTools.endsWith(p, "sys.lua"));
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
		Assert.equals(normalize(newCwd), normalize(Sys.getCwd()));
		Sys.setCwd(cur);
	}
	#end
}
