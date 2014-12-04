package unit.issues;

private abstract ZeroOneFloat(Float) to Float {
    public inline function new(v:Float) {
        this = v<0 ? 0. : (v>1 ? 1. : v);
    }

    @:from public static inline function fromInt(v:Int) {
        return new ZeroOneFloat(v);
    }

    @:from public static inline function fromFloat(v:Float) {
        return new ZeroOneFloat(v);
    }

    @:op(A *= B) public inline function umul_f(r:Float) : ZeroOneFloat {
        return this = new ZeroOneFloat(this*r);
    }

    @:op(A * B) @:commutative public static inline function mul_i(l:ZeroOneFloat, r:Float) return l*r;
}

class Issue2130 extends Test {
	function test() {
        var f : ZeroOneFloat = 0.5;
        feq(0.5, f);
        f *= 2.;
        feq(1, f);


        var f : ZeroOneFloat = 0.5;
        var a : Float = 10.;
		feq(5, a * f);

        a *= f;
        feq(5, a);
	}
}