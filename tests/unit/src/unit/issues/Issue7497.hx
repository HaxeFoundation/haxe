package unit.issues;

import haxe.io.FPHelper;
import Math.isNaN;

class Issue7497 extends unit.Test {
#if js
	static function pot(n) return Math.pow(2, n);
	static function hexI32(b: Int) return StringTools.hex(b, 8);

	static function emuI32ToFloat(i) return @:privateAccess FPHelper._i32ToFloat(i);
	static function emuFloatToI32(f) return @:privateAccess FPHelper._floatToI32(f);

	static function emuDoubleToI64(d) return @:privateAccess FPHelper._doubleToI64(d);
	static function emuI64ToDouble(lo, hi) return @:privateAccess FPHelper._i64ToDouble(lo, hi);

	function rtsingle(i:Int, n:Float, name:String) {
		var ai = emuFloatToI32(n);
		var af = emuI32ToFloat(ai);
		var bi = FPHelper.floatToI32(af);
		var ci = FPHelper.floatToI32(n);
		if (!(isNaN(n) && isNaN(af)))
			feq(n, af);
		eq(ai, bi);
		eq(ai, ci);
	}

	function rtdouble(i:Int, n:Float, name:String){
		var ai = emuDoubleToI64(n);
		var ad = emuI64ToDouble(ai.low, ai.high);
		var bi = FPHelper.doubleToI64(ad);
		var ci = FPHelper.doubleToI64(n);
		if (!(isNaN(n) && isNaN(ad)))
			feq(n, ad);
		eq(ai, bi);
		eq(ai, ci);
	}

	function test() {
		var NA = -9999;
		// float32 to int32
		rtsingle(NA, 0, "s 0");
		rtsingle(NA, -0.0, "s -0");
		rtsingle(NA, pot(-149), "s min");
		rtsingle(NA, -pot(-149), "s -min");
		rtsingle(NA, pot(127) * (2 - pot(-23)), "s max");
		rtsingle(NA, -pot(127) * (2 - pot(-23)), "s -max");
		rtsingle(NA, Math.NEGATIVE_INFINITY, "-infinity");
		rtsingle(NA, Math.POSITIVE_INFINITY, "infinity");
		rtsingle(NA, Math.NaN, "nan");
		for(i in -149...128){ // min denormalized float to max normalized pot float
			var n = pot(i);
			var uulp = if(i < -126) pot(-149) else pot(i - 23);
			var dulp = if(i <= -126) pot(-149) else pot(i - 24);
			rtsingle(i, n, "s pot");
			rtsingle(i, n + uulp, "s pot+");
			rtsingle(i, n - dulp, "s pot-");
			rtsingle(i, n - 2 * dulp, "s pot--");
		}

		// float64 to int64
		rtdouble(NA, 0, "s 0");
		rtdouble(NA, -0.0, "s -0");
		rtdouble(NA, pot(-1075), "d min");
		rtdouble(NA, -pot(-1075), "d -min");
		rtdouble(NA, pot(1023) * (2 - pot( -52)), "d max");
		rtdouble(NA, -pot(1023) * (2 - pot( -52)), "d -max");
		rtdouble(NA, Math.POSITIVE_INFINITY, "d Inf");
		rtdouble(NA, Math.NEGATIVE_INFINITY, "d -Inf");
		rtdouble(NA, Math.NaN, "d NaN");
		for(i in -1075...1024){ // min denormalized double to max normalized pot double
			var n = pot(i);
			var uulp = if(i < -1022) pot(-1075) else pot(i - 52);
			var dulp = if(i <= -1022) pot(-1075) else pot(i - 53);
			rtdouble(i, n, "d pot");
			rtdouble(i, n + 2 * uulp, "d pot+");
			rtdouble(i, n - dulp, "d pot-");
			rtdouble(i, n - 2 * dulp, "d pot--");
		}
	}
#end
}
