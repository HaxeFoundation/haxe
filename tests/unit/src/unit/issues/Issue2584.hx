package unit.issues;
import haxe.ds.Option;

private abstract X (String) to String {
    public function new (s:String) this = s;
}

private abstract XX (String) {
    public function new (s:String) this = s;

	@:to function toString():String {
		return cast this;
	}
}

private abstract X2(X) to X {}

private abstract XArr(Array<X>) to Array<X> {}

private class A {
    public var x : X;
}

private typedef B = {
    var x : String;
}

private typedef B1 = {
    var x(default, null) : String;
}

class Issue2584 extends Test {
	function test() {
		var a : { x: X } = { x: new X("foo") };
		var b : { x: String } = a;
		eq("foo", a.x);
		eq("foo", b.x);

		var a : { x: XX } = { x: new XX("foo") };
		t(unit.TestType.typeError((a : { x: String })));
	}

	function test2() {
		var x : Option<Array<Array<X>>> = null;
		var a : Option<Array<Array<String>>> = x;

		var x : { a : Option<Array<Array<X>>> } = null;
		var a : { a : Option<Array<Array<String>>> } = x;

		var x : A = null;
		var a : B = x;

		var x : A = null;
		var a : B1 = x;

		var x : XArr = null;
		var a : Array<String> = x;

		var x : X2 = null;
		var a : String = x;

		var x : Array<Option<{ x : X}>> = null;
		var a : Array<Option<{ x : String}>> = x;

		var xx : Array<Option<{ x : XX}>> = null;
		t(unit.TestType.typeError((xx : Array<Option<{ x : String}>>)));
	}
}