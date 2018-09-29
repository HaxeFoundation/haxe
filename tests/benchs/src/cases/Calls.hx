package cases;

import haxe.ds.Vector;
import hxbenchmark.Suite;

interface CallInterface {
	function instanceCall0():String;
}

class CallClass implements CallInterface {
	@:pure(false) static public function staticCall0() { return null; }
	@:pure(false) public function instanceCall0() { return null; }
	@:pure(false) public final function finalCall0() { return null; }
	@:pure(false) public function overrideCall0() { return null; }

	public function new() { }
}

class CallClassChild extends CallClass {
	override function overrideCall0() { return null; }
}

class Calls extends TestCase {
	@:analyzer(ignore)
	function measureCreate() {
		var c = new CallClass();
		var cSub:CallClass = new CallClassChild();
		var cInterface:CallInterface = c;
		var cAnon:{ function instanceCall0():String; } = c;
		var cDynamic:Dynamic = c;
		var staticClosureCall0 = CallClass.staticCall0;
		var memberClosureCall0 = c.instanceCall0;
		function closureCall0() { return null; }

		var suite = new Suite("call with 0 args");
		suite.add("static", CallClass.staticCall0());
		suite.add("instance", c.instanceCall0());
		suite.add("override", cSub.overrideCall0());
		suite.add("final", cSub.finalCall0());
		suite.add("interface", cInterface.instanceCall0());
		suite.add("anon", cAnon.instanceCall0());
		suite.add("dynamic", cDynamic.instanceCall0());
		suite.add("local closure", closureCall0());
		suite.add("static closure", staticClosureCall0());
		suite.add("field closure", memberClosureCall0());
		return suite.run();
	}
}