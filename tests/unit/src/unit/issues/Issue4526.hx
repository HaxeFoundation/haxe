package unit.issues;

@:structInit
private class Struct1 {
	var x:Int;
	var y:Int;
	public function get() {
		return x + " " + y;
	}
}

@:structInit
private class Struct2 {
	@:optional var x:Int;
	var y:Int;
	public function get() {
		return x + " " + y;
	}
}

@:structInit
private class Struct3 {
	@:optional var x:Int;
	@:optional var y:Int;
	public function get() {
		return x + " " + y;
	}
}

@:structInit
private class Struct4 {
	var x:Int;
	var y:Int;
	var z:Int;
	public function get() {
		return x + " " + y + " " + z;
	}
}

class Issue4526 extends Test {
	function test() {
		var fieldNull = #if (cpp || flash || java || cs || hl) 0 #else null #end;

		var s1:Struct1 = { x: 12, y: 13 };
		eq("12 13", s1.get());
		t(unit.HelperMacros.typeError(({x:12} : Struct1)));
		t(unit.HelperMacros.typeError(({y:12} : Struct1)));

		var s2:Struct2 = { x: 12, y: 13 };
		eq("12 13", s2.get());
		var s2:Struct2 = { y: 13 };
		eq(fieldNull + " 13", s2.get());
		t(unit.HelperMacros.typeError(({x:12} : Struct2)));

		var s3:Struct3 = { x: 12, y: 13 };
		eq("12 13", s3.get());
		var s3:Struct3 = { y: 13 };
		eq(fieldNull + " 13", s3.get());
		var s3:Struct3 = { };
		eq(fieldNull + " " + fieldNull, s3.get());
	}

	function testOrder() {
		var i = 0;
		var s4:Struct4 = { y: i++, x: i++, z: i++ };
		eq("1 0 2", s4.get());
	}
}