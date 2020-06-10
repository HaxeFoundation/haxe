package unit;

import utest.Assert;

private class MyNotString {
	var s:String;

	public function new(s:String) {
		this.s = s;
	}

	public function toUpperCase() {
		return new MyNotString(s.toUpperCase());
	}

	public function getString() {
		return s;
	}
}

#if java
@:native("unit.DetectiveHaxeExtern")
extern private class DetectiveHaxeExtern {
	@:overload static function itWasYou(i1:Int, i2:Int):String;
	@:overload static function itWasYou(s1:String, s2:String):String;
	@:overload static function itWasYou(f1:Float, f2:Float):String;
}

@:native("unit.DetectiveHaxeExtern")
@:keep
private class DetectiveHaxeImplementation {
	@:overload static function itWasYou(s1:String, s2:String) {
		return s1 + s2;
	}
}
#end

class TestConstrainedMonomorphs extends Test {

	function infer(arg) {
		var s1:MyNotString = arg.toUpperCase();
		var s:MyNotString = arg;
		HelperMacros.typedAs(arg, (null : MyNotString));
		return s.getString() + s1.getString();
	}

	function testNarrowingInference() {
		eq("fooFOO", infer(new MyNotString("foo")));
	}

	#if todo
	function testDetectiveHaxe() {
		var a = null;
		eq("nullfoo", DetectiveHaxeExtern.itWasYou(a, "foo"));
	}
	#end

	static function merge<A:{}, B:{}, C:A & B>(a:A, b:B):C {
		return cast {
			foo: (cast a).foo,
			bar: (cast b).bar
		};
	}

	function testMergedConstraints() {
		var a = merge({foo: 5}, {bar: "bar"});
		eq(5, a.foo);
		eq("bar", a.bar);
		t(HelperMacros.typeError(a.oh));
	}

	public static function returnC<C:{foo:Int}>():C {
		return notMerge();
	}

	public static function notMerge<C:{foo:Int}>():C {
		return null;
	}

}