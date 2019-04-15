package cases;

import haxe.ds.Vector;
import hxbenchmark.Suite;

interface CallInterface {
	function instanceCall0():String;
	function instanceCall1(s1:String):String;
	function instanceCall2(s1:String, s2:String):String;
}

class CallClass implements CallInterface {
	static public function staticCall0() { return null; }
	static public function staticCall1(s1:String) { return null; }
	static public function staticCall2(s1:String, s2:String) { return null; }

	public function instanceCall0() { return null; }
	public function instanceCall1(s1:String) { return null; }
	public function instanceCall2(s1:String, s1:String) { return null; }

	public final function finalCall0() { return null; }
	public final function finalCall1(s1:String) { return null; }
	public final function finalCall2(s1:String, s2:String) { return null; }

	public function overrideCall0() { return null; }
	public function overrideCall1(s1:String) { return null; }
	public function overrideCall2(s2:String, s2:String) { return null; }

	public function new() { }
}

class CallClassChild extends CallClass {
	override function overrideCall0() { return null; }
	override function overrideCall1(s1:String) { return null; }
	override function overrideCall2(s2:String, s2:String) { return null; }
}

class Calls extends TestCase {
	@:analyzer(ignore)
	function measureCall0() {
		var c = new CallClass();
		var cSub:CallClass = new CallClassChild();
		var cInterface:CallInterface = c;
		var cAnon:{ function instanceCall0():String; } = c;
		var cActualAnon = {
			instanceCall0: function ():String {
				return null;
			}
		};
		escape(cActualAnon);
		var cDynamic:Dynamic = c;
		var staticClosureCall0 = CallClass.staticCall0;
		var memberClosureCall0 = c.instanceCall0;
		function closureCall0() { return c; }
		function localFunctionCall0() { return null; }

		var suite = new Suite("call with 0 args");
		suite.add("static", CallClass.staticCall0());
		suite.add("instance", c.instanceCall0());
		suite.add("override", cSub.overrideCall0());
		suite.add("final", cSub.finalCall0());
		suite.add("interface", cInterface.instanceCall0());
		suite.add("class-to-anon", cAnon.instanceCall0());
		suite.add("anon", cActualAnon.instanceCall0());
		suite.add("dynamic", cDynamic.instanceCall0());
		suite.add("local function", localFunctionCall0());
		suite.add("local closure", closureCall0());
		suite.add("static closure", staticClosureCall0());
		suite.add("field closure", memberClosureCall0());
		return suite.run();
	}

	@:analyzer(ignore)
	function measureCall1() {
		var c = new CallClass();
		var cSub:CallClass = new CallClassChild();
		var cInterface:CallInterface = c;
		var cAnon:{
			function instanceCall1(s1:String):String;
		} = c;
		var cActualAnon = {
			instanceCall1: function (s1:String):String {
				return null;
			}
		};
		escape(cActualAnon);
		var cDynamic:Dynamic = c;
		var staticClosureCall1 = CallClass.staticCall1;
		var memberClosureCall1 = c.instanceCall1;
		function closureCall1(s1:String) { return c; }
		function localFunctionCall1(s1:String) { return null; }

		var suite = new Suite("call with 1 arg");
		suite.add("static", CallClass.staticCall1("foo"));
		suite.add("instance", c.instanceCall1("foo"));
		suite.add("override", cSub.overrideCall1("foo"));
		suite.add("final", cSub.finalCall1("foo"));
		suite.add("interface", cInterface.instanceCall1("foo"));
		suite.add("class-to-anon", cAnon.instanceCall1("foo"));
		suite.add("anon", cActualAnon.instanceCall1("foo"));
		suite.add("dynamic", cDynamic.instanceCall1("foo"));
		suite.add("local function", localFunctionCall1("foo"));
		suite.add("local closure", closureCall1("foo"));
		suite.add("static closure", staticClosureCall1("foo"));
		suite.add("field closure", memberClosureCall1("foo"));
		return suite.run();
	}

	@:analyzer(ignore)
	function measureCall2() {
		var c = new CallClass();
		var cSub:CallClass = new CallClassChild();
		var cInterface:CallInterface = c;
		var cAnon:{
			function instanceCall2(s1:String, s2:String):String;
		} = c;
		var cActualAnon = {
			instanceCall2: function (s1:String, s2:String):String {
				return null;
			}
		};
		escape(cActualAnon);
		var cDynamic:Dynamic = c;
		var staticClosureCall2 = CallClass.staticCall2;
		var memberClosureCall2 = c.instanceCall2;
		function closureCall2(s1:String, s2:String) { return c; }
		function localFunctionCall2(s1:String, s2:String) { return null; }

		var suite = new Suite("call with 2 args");
		suite.add("static", CallClass.staticCall2("foo", "bar"));
		suite.add("instance", c.instanceCall2("foo", "bar"));
		suite.add("override", cSub.overrideCall2("foo", "bar"));
		suite.add("final", cSub.finalCall2("foo", "bar"));
		suite.add("interface", cInterface.instanceCall2("foo", "bar"));
		suite.add("class-to-anon", cAnon.instanceCall2("foo", "bar"));
		suite.add("anon", cActualAnon.instanceCall2("foo", "bar"));
		suite.add("dynamic", cDynamic.instanceCall2("foo", "bar"));
		suite.add("local function", localFunctionCall2("foo", "bar"));
		suite.add("local closure", closureCall2("foo", "bar"));
		suite.add("static closure", staticClosureCall2("foo", "bar"));
		suite.add("field closure", memberClosureCall2("foo", "bar"));
		return suite.run();
	}

	static function escape(d:Dynamic) { }
}