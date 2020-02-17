package unit.issues;

class Issue8549 extends unit.Test {
	#if flash
	function test() {
		// can be used as a Vector type param, for type checking :-/
		var v = new flash.Vector<String>();
		t(Std.isOfType(v, (flash.Vector.typeReference() : Class<flash.Vector<flash.AnyType>>)));

		// also assignable from/to stuff, similar to Any, just in case...
		var v:flash.AnyType = 10;
		eq(10, v);
		var i:Int = v;
		eq(10, i);
	}
	#end
}