package unit.issues;

#if lua
@:multiReturn
private extern class MRValue {
	var a:Int;
	var b:Int;
}

private class MRHelper {
	public static function getValue():Dynamic {
		return untyped __lua__("10, 15");
	}
}
#end

class Issue12540 extends Test {
	#if lua
	// Test that multiReturn works in __init__ - before fix, this would fail
	// with "attempt to index a number value"
	static var initA:Int;
	static var initB:Int;

	static function __init__() {
		var v:MRValue = untyped MRHelper.getValue();
		initA = v.a;
		initB = v.b;
	}

	function test() {
		// Verify multiReturn field access worked in __init__
		eq(initA, 10);
		eq(initB, 15);
		// Also verify it works in regular methods
		var v:MRValue = untyped MRHelper.getValue();
		eq(v.a, 10);
		eq(v.b, 15);
	}
	#end
}
