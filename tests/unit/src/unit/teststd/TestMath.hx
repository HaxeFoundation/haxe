package unit.teststd;

class TestMath extends unit.Test {
	public function test() {
		// constants
		var zero = 0.0;
		var one = 1.0;
		//1.0 / zero == Math.POSITIVE_INFINITY;
		//-1.0 / zero == Math.NEGATIVE_INFINITY;
		f((Math.NaN == Math.NaN));
		t(Math.isNaN(Math.NaN));
		t(Math.isNaN(Math.sqrt( -1)));
		feq(Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY);
		feq(Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY);
		#if !python
		t(Math.isNaN(0/0));
		#end
		// +
		feq(Math.POSITIVE_INFINITY + Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY + Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY);
		feq(Math.POSITIVE_INFINITY + one, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY + one, Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.POSITIVE_INFINITY + Math.NEGATIVE_INFINITY));
		t(Math.isNaN(Math.POSITIVE_INFINITY + Math.NaN));
		t(Math.isNaN(Math.NEGATIVE_INFINITY + Math.NaN));
		// -
		feq(one - Math.POSITIVE_INFINITY, Math.NEGATIVE_INFINITY);
		feq(one - Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(-Math.POSITIVE_INFINITY, Math.NEGATIVE_INFINITY);
		feq(-Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(Math.POSITIVE_INFINITY - one, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY - one, Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.POSITIVE_INFINITY - Math.POSITIVE_INFINITY ));
		t(Math.isNaN(Math.NEGATIVE_INFINITY - Math.NEGATIVE_INFINITY));
		feq(Math.POSITIVE_INFINITY - Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY - Math.POSITIVE_INFINITY, Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.POSITIVE_INFINITY - Math.NaN));
		t(Math.isNaN(Math.NEGATIVE_INFINITY - Math.NaN));
		t(Math.isNaN(Math.NaN - Math.POSITIVE_INFINITY));
		t(Math.isNaN(Math.NaN - Math.NEGATIVE_INFINITY));
		// *
		feq(Math.POSITIVE_INFINITY * one, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY * one, Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.POSITIVE_INFINITY * zero));
		t(Math.isNaN(Math.NEGATIVE_INFINITY * zero));
		feq(Math.POSITIVE_INFINITY * Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY * Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY);
		feq(Math.POSITIVE_INFINITY * Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.POSITIVE_INFINITY * Math.NaN));
		t(Math.isNaN(Math.NEGATIVE_INFINITY * Math.NaN));
		// /
		feq(Math.POSITIVE_INFINITY / one, Math.POSITIVE_INFINITY);
		feq(Math.NEGATIVE_INFINITY / one, Math.NEGATIVE_INFINITY);
		//Math.POSITIVE_INFINITY / zero == Math.POSITIVE_INFINITY;
		//Math.NEGATIVE_INFINITY / zero == Math.NEGATIVE_INFINITY;
		t(Math.isNaN(Math.POSITIVE_INFINITY / Math.POSITIVE_INFINITY));
		t(Math.isNaN(Math.POSITIVE_INFINITY / Math.NEGATIVE_INFINITY));
		t(Math.isNaN(Math.NEGATIVE_INFINITY / Math.POSITIVE_INFINITY));
		t(Math.isNaN(Math.NEGATIVE_INFINITY / Math.NEGATIVE_INFINITY));
		t(Math.isNaN(Math.NaN / Math.POSITIVE_INFINITY));
		t(Math.isNaN(Math.POSITIVE_INFINITY / Math.NaN));
		t(Math.isNaN(Math.NaN / Math.POSITIVE_INFINITY));
		t(Math.isNaN(Math.NEGATIVE_INFINITY / Math.NaN));

		// %
		// var izero = 0;
		// Math.isNaN(1%izero) == true;
		// abs
		feq(Math.abs(-1.223), 1.223);
		feq(Math.abs(1.223), 1.223);
		feq(Math.abs(0), 0);
		t(Math.isNaN(Math.abs(Math.NaN)));
		feq(Math.abs(Math.NEGATIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.abs(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);

		// min
		feq(Math.min(0.0, 1.0), 0.0);
		feq(Math.min(0.0, -1.0), -1.0);
		feq(Math.min(0.0, 0.0), 0.0);
		feq(Math.min(1.0, 1.0), 1.0);
		feq(Math.min(Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY), Math.NEGATIVE_INFINITY);
		feq(Math.min(Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY), Math.NEGATIVE_INFINITY);
		feq(Math.min(Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.min(Math.POSITIVE_INFINITY, zero), zero);
		feq(Math.min(Math.NEGATIVE_INFINITY, zero), Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.min(Math.NEGATIVE_INFINITY, Math.NaN)));
		t(Math.isNaN(Math.min(Math.POSITIVE_INFINITY, Math.NaN)));
		t(Math.isNaN(Math.min(Math.NaN, Math.NaN)));
		t(Math.isNaN(Math.min(one, Math.NaN)));
		t(Math.isNaN(Math.min(zero, Math.NaN)));
		t(Math.isNaN(Math.min(Math.NaN, Math.NEGATIVE_INFINITY)));
		t(Math.isNaN(Math.min(Math.NaN,Math.POSITIVE_INFINITY)));
		t(Math.isNaN(Math.min(Math.NaN, one)));
		t(Math.isNaN(Math.min(Math.NaN, zero)));

		// max
		feq(Math.max(0.0, 1.0), 1.0);
		feq(Math.max(0.0, -1.0), 0.0);
		feq(Math.max(0.0, 0.0), 0.0);
		feq(Math.max(1.0, 1.0), 1.0);
		feq(Math.max(Math.NEGATIVE_INFINITY, Math.NEGATIVE_INFINITY), Math.NEGATIVE_INFINITY);
		feq(Math.max(Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.max(Math.POSITIVE_INFINITY, Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.max(Math.POSITIVE_INFINITY, zero), Math.POSITIVE_INFINITY);
		feq(Math.max(Math.NEGATIVE_INFINITY, zero), 0);
		t(Math.isNaN(Math.max(Math.NEGATIVE_INFINITY, Math.NaN)));
		t(Math.isNaN(Math.max(Math.POSITIVE_INFINITY, Math.NaN)));
		t(Math.isNaN(Math.max(Math.NaN, Math.NaN)));
		t(Math.isNaN(Math.max(one, Math.NaN)));
		t(Math.isNaN(Math.max(zero, Math.NaN)));
		t(Math.isNaN(Math.max(Math.NaN, Math.NEGATIVE_INFINITY)));
		t(Math.isNaN(Math.max(Math.NaN,Math.POSITIVE_INFINITY)));
		t(Math.isNaN(Math.max(Math.NaN, one)));
		t(Math.isNaN(Math.max(Math.NaN, zero)));

		// sin
		feq(Math.sin(0.0), 0.0);
		feq(Math.sin(Math.PI / 2), 1.0);
		feq(Math.sin(Math.PI), 0.0);
		feq(Math.sin(Math.PI * 3 / 2), -1.0);
		t(Math.isNaN(Math.sin(Math.POSITIVE_INFINITY)));
		t(Math.isNaN(Math.sin(Math.NEGATIVE_INFINITY)));
		t(Math.isNaN(Math.sin(Math.NaN)));

		// cos
		feq(Math.cos(0.0), 1.0);
		feq(Math.cos(Math.PI / 2), 0.0);
		feq(Math.cos(Math.PI), -1.0);
		feq(Math.cos(Math.PI * 3 / 2), 0.0);
		t(Math.isNaN(Math.cos(Math.POSITIVE_INFINITY)));
		t(Math.isNaN(Math.cos(Math.NEGATIVE_INFINITY)));
		t(Math.isNaN(Math.cos(Math.NaN)));

		// exp
		feq(Math.exp(0.0), 1.0);
		feq(Math.exp(1.0), 2.7182818284590452353602874713527);
		feq(Math.exp(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.exp(Math.NEGATIVE_INFINITY), 0.0);
		t(Math.isNaN(Math.exp(Math.NaN)));

		// log
		feq(Math.log(0.0), Math.NEGATIVE_INFINITY);
		feq(Math.log(2.7182818284590452353602874713527), 1.0);
		t(Math.isNaN(Math.log( -1.0)));
		t(Math.isNaN(Math.log(Math.NaN)));
		t(Math.isNaN(Math.log(Math.NEGATIVE_INFINITY)));
		feq(Math.log(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);

		// exp + log
		var floats = [1.33, 12.0, -112.999992, 0.0, Math.NEGATIVE_INFINITY, Math.POSITIVE_INFINITY];
		for (f in floats) {
			feq(Math.log(Math.exp(f)), f);
		}

		// sqrt
		feq(Math.sqrt(4.0), 2);
		feq(Math.sqrt(0.0), 0.0);
		feq(Math.sqrt(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		t(Math.isNaN(Math.sqrt(Math.NEGATIVE_INFINITY)));
		t(Math.isNaN(Math.sqrt(Math.NaN)));
		t(Math.isNaN(Math.sqrt( -1.0)));

		// round
		eq(Math.round(0.0), 0);
		eq(Math.round(0.1), 0);
		eq(Math.round(0.4999), 0);
		eq(Math.round(0.5), 1);
		eq(Math.round(1.0), 1);
		eq(Math.round(1.499), 1);
		eq(Math.round(-0.1), 0);
		eq(Math.round(-0.4999), 0);
		eq(Math.round(-0.5), 0);
		eq(Math.round(-0.50001), -1);
		eq(Math.round(-1.0), -1);
		eq(Math.round(-1.499), -1);
		eq(Math.round(-1.5), -1);
		eq(Math.round( -1.50001), -2);
		feq(Math.fround(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.fround(Math.NEGATIVE_INFINITY), Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.fround(Math.NaN)));
		feq(Math.fround(0.0), 0.0);
		feq(Math.fround(0.1), 0.0);
		feq(Math.fround(0.4999), 0.0);
		feq(Math.fround(0.5), 1.0);
		feq(Math.fround(1.0), 1.0);
		feq(Math.fround(1.499), 1.0);
		feq(Math.fround(1.5), 2.0);
		feq(Math.fround(-0.1), -0.0);
		feq(Math.fround(-0.4999), -0.0);
		feq(Math.fround(-0.5), -0.0);
		feq(Math.fround(-0.50001), -1.0);
		feq(Math.fround(-1.0), -1.0);
		feq(Math.fround(-1.499), -1.0);
		feq(Math.fround(-1.5), -1.0);
		feq(Math.fround( -1.50001), -2.0);

		// floor
		eq(Math.floor(0.0), 0);
		eq(Math.floor(0.9999), 0);
		eq(Math.floor(1.0), 1);
		eq(Math.floor( -0.0001), -1);
		eq(Math.floor( -1.0), -1);
		eq(Math.floor( -1.0001), -2);
		feq(Math.ffloor(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.ffloor(Math.NEGATIVE_INFINITY), Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.ffloor(Math.NaN)));

		// ceil
		eq(Math.ceil(0.0), 0);
		eq(Math.ceil(-0.9999), 0);
		eq(Math.ceil(-1.0), -1);
		eq(Math.ceil( 0.0001), 1);
		eq(Math.ceil( 1.0), 1);
		eq(Math.ceil( 1.0001), 2);
		feq(Math.fceil(Math.POSITIVE_INFINITY), Math.POSITIVE_INFINITY);
		feq(Math.fceil(Math.NEGATIVE_INFINITY), Math.NEGATIVE_INFINITY);
		t(Math.isNaN(Math.fceil(Math.NaN)));

		// random
		// not much to test here...

		// isFinite
		f(Math.isFinite(Math.POSITIVE_INFINITY));
		f(Math.isFinite(Math.NEGATIVE_INFINITY));
		f(Math.isFinite(Math.NaN));
		t(Math.isFinite(0.0));

		// isNaN
		f(Math.isNaN(Math.POSITIVE_INFINITY));
		f(Math.isNaN(Math.NEGATIVE_INFINITY));
		t(Math.isNaN(Math.NaN));
		f(Math.isNaN(0.0));


		// Dynamic version
		var math = Math;


		//1.0 / zero == math.POSITIVE_INFINITY;
		//-1.0 / zero == math.NEGATIVE_INFINITY;
		f((math.NaN == math.NaN));
		t(math.isNaN(math.NaN));
		t(math.isNaN(math.sqrt( -1)));
		feq(math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY);
		feq(math.POSITIVE_INFINITY, math.POSITIVE_INFINITY);
		// +
		feq(math.POSITIVE_INFINITY + math.POSITIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY + math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY);
		feq(math.POSITIVE_INFINITY + one, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY + one, math.NEGATIVE_INFINITY);
		t(math.isNaN(math.POSITIVE_INFINITY + math.NEGATIVE_INFINITY));
		t(math.isNaN(math.POSITIVE_INFINITY + math.NaN));
		t(math.isNaN(math.NEGATIVE_INFINITY + math.NaN));
		// -
		feq(one - math.POSITIVE_INFINITY, math.NEGATIVE_INFINITY);
		feq(one - math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(-math.POSITIVE_INFINITY, math.NEGATIVE_INFINITY);
		feq(-math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(math.POSITIVE_INFINITY - one, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY - one, math.NEGATIVE_INFINITY);
		t(math.isNaN(math.POSITIVE_INFINITY - math.POSITIVE_INFINITY ));
		t(math.isNaN(math.NEGATIVE_INFINITY - math.NEGATIVE_INFINITY));
		feq(math.POSITIVE_INFINITY - math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY - math.POSITIVE_INFINITY, math.NEGATIVE_INFINITY);
		t(math.isNaN(math.POSITIVE_INFINITY - math.NaN));
		t(math.isNaN(math.NEGATIVE_INFINITY - math.NaN));
		t(math.isNaN(math.NaN - math.POSITIVE_INFINITY));
		t(math.isNaN(math.NaN - math.NEGATIVE_INFINITY));
		// *
		feq(math.POSITIVE_INFINITY * one, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY * one, math.NEGATIVE_INFINITY);
		t(math.isNaN(math.POSITIVE_INFINITY * zero));
		t(math.isNaN(math.NEGATIVE_INFINITY * zero));
		feq(math.POSITIVE_INFINITY * math.POSITIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY * math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY);
		feq(math.POSITIVE_INFINITY * math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY);
		t(math.isNaN(math.POSITIVE_INFINITY * math.NaN));
		t(math.isNaN(math.NEGATIVE_INFINITY * math.NaN));
		// /
		feq(math.POSITIVE_INFINITY / one, math.POSITIVE_INFINITY);
		feq(math.NEGATIVE_INFINITY / one, math.NEGATIVE_INFINITY);
		//math.POSITIVE_INFINITY / zero == math.POSITIVE_INFINITY;
		//math.NEGATIVE_INFINITY / zero == math.NEGATIVE_INFINITY;
		t(math.isNaN(math.POSITIVE_INFINITY / math.POSITIVE_INFINITY));
		t(math.isNaN(math.POSITIVE_INFINITY / math.NEGATIVE_INFINITY));
		t(math.isNaN(math.NEGATIVE_INFINITY / math.POSITIVE_INFINITY));
		t(math.isNaN(math.NEGATIVE_INFINITY / math.NEGATIVE_INFINITY));
		t(math.isNaN(math.NaN / math.POSITIVE_INFINITY));
		t(math.isNaN(math.POSITIVE_INFINITY / math.NaN));
		t(math.isNaN(math.NaN / math.POSITIVE_INFINITY));
		t(math.isNaN(math.NEGATIVE_INFINITY / math.NaN));

		// abs
		feq(math.abs(-1.223), 1.223);
		feq(math.abs(1.223), 1.223);
		feq(math.abs(0), 0);
		t(math.isNaN(math.abs(math.NaN)));
		feq(math.abs(math.NEGATIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.abs(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);

		// min
		feq(math.min(0.0, 1.0), 0.0);
		feq(math.min(0.0, -1.0), -1.0);
		feq(math.min(0.0, 0.0), 0.0);
		feq(math.min(1.0, 1.0), 1.0);
		feq(math.min(math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY), math.NEGATIVE_INFINITY);
		feq(math.min(math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY), math.NEGATIVE_INFINITY);
		feq(math.min(math.POSITIVE_INFINITY, math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.min(math.POSITIVE_INFINITY, zero), zero);
		feq(math.min(math.NEGATIVE_INFINITY, zero), math.NEGATIVE_INFINITY);
		t(math.isNaN(math.min(math.NEGATIVE_INFINITY, math.NaN)));
		t(math.isNaN(math.min(math.POSITIVE_INFINITY, math.NaN)));
		t(math.isNaN(math.min(math.NaN, math.NaN)));
		t(math.isNaN(math.min(one, math.NaN)));
		t(math.isNaN(math.min(zero, math.NaN)));
		t(math.isNaN(math.min(math.NaN, math.NEGATIVE_INFINITY)));
		t(math.isNaN(math.min(math.NaN,math.POSITIVE_INFINITY)));
		t(math.isNaN(math.min(math.NaN, one)));
		t(math.isNaN(math.min(math.NaN, zero)));

		// max
		feq(math.max(0.0, 1.0), 1.0);
		feq(math.max(0.0, -1.0), 0.0);
		feq(math.max(0.0, 0.0), 0.0);
		feq(math.max(1.0, 1.0), 1.0);
		feq(math.max(math.NEGATIVE_INFINITY, math.NEGATIVE_INFINITY), math.NEGATIVE_INFINITY);
		feq(math.max(math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.max(math.POSITIVE_INFINITY, math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.max(math.POSITIVE_INFINITY, zero), math.POSITIVE_INFINITY);
		feq(math.max(math.NEGATIVE_INFINITY, zero), 0);
		t(math.isNaN(math.max(math.NEGATIVE_INFINITY, math.NaN)));
		t(math.isNaN(math.max(math.POSITIVE_INFINITY, math.NaN)));
		t(math.isNaN(math.max(math.NaN, math.NaN)));
		t(math.isNaN(math.max(one, math.NaN)));
		t(math.isNaN(math.max(zero, math.NaN)));
		t(math.isNaN(math.max(math.NaN, math.NEGATIVE_INFINITY)));
		t(math.isNaN(math.max(math.NaN,math.POSITIVE_INFINITY)));
		t(math.isNaN(math.max(math.NaN, one)));
		t(math.isNaN(math.max(math.NaN, zero)));

		// sin
		feq(math.sin(0.0), 0.0);
		feq(math.sin(math.PI / 2), 1.0);
		feq(math.sin(math.PI), 0.0);
		feq(math.sin(math.PI * 3 / 2), -1.0);
		t(math.isNaN(math.sin(math.POSITIVE_INFINITY)));
		t(math.isNaN(math.sin(math.NEGATIVE_INFINITY)));
		t(math.isNaN(math.sin(math.NaN)));

		// cos
		feq(math.cos(0.0), 1.0);
		feq(math.cos(math.PI / 2), 0.0);
		feq(math.cos(math.PI), -1.0);
		feq(math.cos(math.PI * 3 / 2), 0.0);
		t(math.isNaN(math.cos(math.POSITIVE_INFINITY)));
		t(math.isNaN(math.cos(math.NEGATIVE_INFINITY)));
		t(math.isNaN(math.cos(math.NaN)));

		// exp
		feq(math.exp(0.0), 1.0);
		feq(math.exp(1.0), 2.7182818284590452353602874713527);
		feq(math.exp(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.exp(math.NEGATIVE_INFINITY), 0.0);
		t(math.isNaN(math.exp(math.NaN)));

		// log
		feq(math.log(0.0), math.NEGATIVE_INFINITY);
		feq(math.log(2.7182818284590452353602874713527), 1.0);
		t(math.isNaN(math.log( -1.0)));
		t(math.isNaN(math.log(math.NaN)));
		t(math.isNaN(math.log(math.NEGATIVE_INFINITY)));
		feq(math.log(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);

		// exp + log
		var floats = [1.33, 12.0, -112.999992, 0.0, math.NEGATIVE_INFINITY, math.POSITIVE_INFINITY];
		for (f in floats) {
			feq(math.log(math.exp(f)), f);
		}

		// sqrt
		feq(math.sqrt(4.0), 2);
		feq(math.sqrt(0.0), 0.0);
		feq(math.sqrt(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		t(math.isNaN(math.sqrt(math.NEGATIVE_INFINITY)));
		t(math.isNaN(math.sqrt(math.NaN)));
		t(math.isNaN(math.sqrt( -1.0)));

		// round
		eq(math.round(0.0), 0);
		eq(math.round(0.1), 0);
		eq(math.round(0.4999), 0);
		eq(math.round(0.5), 1);
		eq(math.round(1.0), 1);
		eq(math.round(1.499), 1);
		eq(math.round(-0.1), 0);
		eq(math.round(-0.4999), 0);
		eq(math.round(-0.5), 0);
		eq(math.round(-0.50001), -1);
		eq(math.round(-1.0), -1);
		eq(math.round(-1.499), -1);
		eq(math.round(-1.5), -1);
		eq(math.round( -1.50001), -2);
		feq(math.fround(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.fround(math.NEGATIVE_INFINITY), math.NEGATIVE_INFINITY);
		t(math.isNaN(math.fround(math.NaN)));

		// floor
		eq(math.floor(0.0), 0);
		eq(math.floor(0.9999), 0);
		eq(math.floor(1.0), 1);
		eq(math.floor( -0.0001), -1);
		eq(math.floor( -1.0), -1);
		eq(math.floor( -1.0001), -2);
		feq(math.ffloor(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.ffloor(math.NEGATIVE_INFINITY), math.NEGATIVE_INFINITY);
		t(math.isNaN(math.ffloor(math.NaN)));

		// ceil
		eq(math.ceil(0.0), 0);
		eq(math.ceil(-0.9999), 0);
		eq(math.ceil(-1.0), -1);
		eq(math.ceil( 0.0001), 1);
		eq(math.ceil( 1.0), 1);
		eq(math.ceil( 1.0001), 2);
		feq(math.fceil(math.POSITIVE_INFINITY), math.POSITIVE_INFINITY);
		feq(math.fceil(math.NEGATIVE_INFINITY), math.NEGATIVE_INFINITY);
		t(math.isNaN(math.fceil(math.NaN)));

		// random
		// not much to test here...

		// isFinite
		f(math.isFinite(math.POSITIVE_INFINITY));
		f(math.isFinite(math.NEGATIVE_INFINITY));
		f(math.isFinite(math.NaN));
		t(math.isFinite(0.0));

		// isNaN
		f(math.isNaN(math.POSITIVE_INFINITY));
		f(math.isNaN(math.NEGATIVE_INFINITY));
		t(math.isNaN(math.NaN));
		f(math.isNaN(0.0));

		// atan2
		feq(math.atan2(0,1), 0);
		feq(math.atan2(0,1000), 0);
		feq(math.atan2(1,0), Math.PI/2);
		feq(math.atan2(-1,0), -Math.PI/2);
		feq(math.atan2(0,0), 0);

	}
}
