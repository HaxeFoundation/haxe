package unit.issues;

#if js
@:native("some6448")
private class C {
	public var v = true;
	public function new() {}
}

@:native("")
private extern class Lib {
	@:native("___hx_returnTrue")
	static function returnTrue():Bool;

	static function __init__():Void {
		js.Syntax.code("function ___hx_returnTrue() { return true; }");
	}
}
#end

class Issue6448 extends unit.Test {
	#if js
	@:analyzer(no_local_dce)
	function test() {
		var some6448 = null;
		t(new C().v);

		function ___hx_returnTrue() return false;
		t(Lib.returnTrue());
	}
	#end
}
