import utest.Assert;

class TestSys extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		return Sys.command(cmd, args);
	}

	function testEnvironment() {
		var env = Sys.environment();
		// EXISTS should be set manually via the command line
		Assert.notNull(env.get("EXISTS"));
		Assert.isNull(env.get("doesn't exist"));
	}

	function testGetEnv() {
		// EXISTS should be set manually via the command line
		Assert.notNull(Sys.getEnv("EXISTS"));
		Assert.isNull(Sys.getEnv("doesn't exist"));
	}

	#if !java
	function testPutEnv() {
		Sys.putEnv("foo", "value");
		Assert.equals("value", Sys.getEnv("foo"));

		var env = Sys.environment();
		Assert.equals("value", env.get("foo"));

		// null
		Sys.putEnv("foo", null);
		Assert.isNull(Sys.getEnv("foo"));

		#if !(python || cs) // #10401
		env = Sys.environment();
		#end

		Assert.isFalse(env.exists("foo"));
	}
	#end

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

	function testGetCwd() {
		final current = Sys.getCwd();
		// ensure it has a trailing slash
		Assert.notEquals(current, haxe.io.Path.removeTrailingSlashes(current));
	}

	#if !java
	function testSetCwd() {
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
