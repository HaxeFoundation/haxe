class TestSys extends haxe.unit.TestCase {
	function testArgs() {
		var args = Sys.args();
		assertEquals(3, args.length);
		assertEquals("foo", args[0]);
		assertEquals("12", args[1]);
		assertEquals("a b c\\ &<>[\"]#{}|", args[2]);		
	}

	function testEnv() {
		Sys.putEnv("foo", "value");
		assertEquals("value", Sys.getEnv("foo"));
		assertEquals(null, Sys.getEnv("doesn't exist"));

		var env = Sys.environment();
		assertEquals("value", env.get("foo"));
	}

	function testCwd() {
		var cur = Sys.getCwd();
		Sys.setCwd("../");
		var newCwd = haxe.io.Path.join([cur, "../"]);
		function normalize(path) {
			return haxe.io.Path.addTrailingSlash(haxe.io.Path.normalize(path));
		}
		assertEquals(normalize(newCwd), normalize(Sys.getCwd()));
	}
}