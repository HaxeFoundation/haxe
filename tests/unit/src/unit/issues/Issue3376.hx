package unit.issues;

private abstract Vec3(Array<Float>) from Array<Float> to Array<Float> {
	inline public function new(_x:Float, _y:Float, _z:Float):Vec3 {
		this = [_x, _y, _z];
	}

	@:op(A == B)
	public static function eq1(lhs:Vec3, rhs:Vec3):String {
		return "eq1";
	}

	@:op(A == B)
	public static function eq2(lhs:Vec3, rhs:Null<Vec3>):String {
		return "eq2";
	}

	@:op(A == B)
	public static function eq3(lhs:Null<Vec3>, rhs:Vec3):String {
		return "eq3";
	}

	@:op(A != B)
	public static function neq1(lhs:Vec3, rhs:Vec3):String {
		return "neq1";
	}

	@:op(A != B)
	public static function neq2(lhs:Vec3, rhs:Null<Vec3>):String {
		return "neq2";
	}
}

class Issue3376 extends Test {
	function test() {
		var vnull:Vec3 = null;
		var v:Vec3 = new Vec3(1, 2, 3);
		eq("eq1", v == v);
		eq("eq2", vnull == null);
		eq("eq3", null == vnull);
		eq("neq1", v != v);
		eq("neq2", vnull != null);
		eq(false, null != vnull);
	}
}