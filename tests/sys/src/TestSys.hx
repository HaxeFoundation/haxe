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

		final nonExistent = "NON_EXISTENT";
		env.set(nonExistent, "1");
		// new copies should not be affected
		Assert.isNull(Sys.environment()[nonExistent]);

		#if !java
		// env should not update when environment updates
		final toUpdate = "TO_UPDATE";

		Sys.putEnv(toUpdate, "1");
		Assert.isNull(env.get(toUpdate));

		// new copy should have the variable
		Assert.equals("1", Sys.environment()[toUpdate]);

		// environment should not update if env updates
		env.set(toUpdate, "2");
		Assert.equals("1", Sys.getEnv(toUpdate));

		// variables set via target specific api should exist
		#if (cs || python)
		final toSetNatively = "SET_NATIVELY";
		#if cs
		cs.system.Environment.SetEnvironmentVariable(toSetNatively, "1");
		#elseif python
		python.lib.Os.environ.set(toSetNatively, "1");
		#end
		Assert.equals("1", Sys.environment()[toSetNatively]);
		#end
		#end
	}

	function existsInSubProcess(variable:String, value:String) {
		#if js
		return UtilityProcess.runUtilityAsCommand(["checkEnv", variable, value]) == 0;
		#else
		return UtilityProcess.runUtility(["checkEnv", variable, value]).exitCode == 0;
		#end
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

		Assert.equals("value", Sys.environment().get("foo"));

		Assert.isTrue(existsInSubProcess("foo", "value"));

		#if python
		// the variable should also be visible through python's api
		Assert.equals("value", python.lib.Os.environ.get("foo"));
		#end

		// null
		Sys.putEnv("foo", null);
		Assert.isNull(Sys.getEnv("foo"));

		Assert.isFalse(Sys.environment().exists("foo"));

		Assert.isFalse(existsInSubProcess("foo", "value"));

		#if python
		// the variable should also be gone when checking through python's api
		Assert.isFalse(python.lib.Os.environ.hasKey("foo"));
		#end
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
