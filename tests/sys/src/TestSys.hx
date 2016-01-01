class TestSys extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		return Sys.command(cmd, args);
	}

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
