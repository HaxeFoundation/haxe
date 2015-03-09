class TestSys extends haxe.unit.TestCase {
	// it receives the arguments passed to Haxe command line
	#if !interp
	function testArgs() {
		var args = Sys.args();
		var expectedArgs = ~/\r?\n/g.split(haxe.Resource.getString("args.txt"));
		// trace(args);
		assertEquals(expectedArgs.length, args.length);
		for (i in 0...expectedArgs.length) {
			assertEquals(expectedArgs[i], args[i]);
		}
	}
	#end

	function testEnv() {
		#if !java
		Sys.putEnv("foo", "value");
		assertEquals("value", Sys.getEnv("foo"));
		#end
		assertEquals(null, Sys.getEnv("doesn't exist"));

		#if !java
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
	}
	#end
}
