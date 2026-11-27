package unit.issues;

@:nullSafety
@:structInit
private class Struct {
	@:optional public var x:Int;
	@:optional public var y:Int = 42;
	@:optional public var z(default, null):Int;

	public function getX():Null<Int> {
		return x;
	}
}

class Issue13240 extends Test {
	function test() {
		var s:Struct = {};
		eq(null, s.getX());
		eq(42, s.y);
		t(unit.HelperMacros.typeString(s.x) == "Null<Int>");
		t(unit.HelperMacros.typeString(s.y) == "Int");
		t(unit.HelperMacros.typeString(s.z) == "Null<Int>");
	}
}
