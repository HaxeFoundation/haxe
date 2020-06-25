package unit.issues;

class Issue7736 extends unit.Test {
	function testNullBasicType() {
		// this test checks that an explicit null, assigned to the basic type
		// behaves exactly the same as if Null<T>-typed null would be assigned to basic type

		var i:Int = null;
		var f:Float = null;
		var b:Bool = null;

		eq(i, getDefaultInt());
		eq(f, getDefaultFloat());
		eq(b, getDefaultBool());
	}

	static function getDefaultInt():Int return (null : Null<Int>);
	static function getDefaultFloat():Float return (null : Null<Float>);
	static function getDefaultBool():Bool return (null : Null<Bool>);
}
