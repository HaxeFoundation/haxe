package unit.issues;

private abstract Vec3(Array<Float>) from Array<Float> to Array<Float> {
    inline public function new(_x:Float, _y:Float, _z:Float):Vec3
    	this = _alloc(_x, _y, _z);

	inline private static function _alloc(_x:Float, _y:Float, _z:Float):Array<Float>
        return [_x, _y, _z];
}

private abstract Quat(Array<Float>) from Array<Float> to Array<Float> {
    inline public function new(_x:Float, _y:Float, _z:Float, _w:Float):Quat
    	this = _alloc(_x, _y, _z, _w);

	inline private static function _alloc(_x:Float, _y:Float, _z:Float, _w:Float):Array<Float>
        return [_x, _y, _z, _w];

    @:op(A * B)
    inline public static function transformVec3(lhs:Quat, rhs:Vec3):Vec3 {
        return new Vec3(1,2,3);
    }

    @:op(A * B)
	inline public static function mult(lhs:Quat, rhs:Quat):Quat {
        return new Quat(1,2,3,1);
    }
}

private abstract Kilometer(Float) from Float to Float { }
private abstract Meter(Float) from Float { }

class Issue3360 extends Test {
	function test() {
        var v = new Vec3(0,0,0);
        var q0 = new Quat(0,0,0,1);
        var q1 = new Quat(0,0,0,1);

        var res = q0 * v;
		unit.TestType.typedAs(res, (null : Vec3));
		feq(1, res[0]);
		feq(2, res[1]);
		feq(3, res[2]);
        var res = q0 * q1;
		unit.TestType.typedAs(res, (null : Quat));
		feq(1, res[0]);
		feq(2, res[1]);
		feq(3, res[2]);
		feq(1, res[3]);

		var k:Kilometer = 1;
		t(unit.TestType.typeError(var m:Meter = k));
	}
}