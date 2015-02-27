class TestSys extends haxe.unit.TestCase {
	// it receives the arguments passed to Haxe command line
	#if !interp
	function testArgs() {
		var args = Sys.args();
		trace(args);
		assertEquals(3, args.length);
		assertEquals("foo", args[0]);
		assertEquals("12", args[1]);
		assertEquals("a b  %PATH% $HOME c\\&<>[\"]#{}|%$", args[2]);
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
