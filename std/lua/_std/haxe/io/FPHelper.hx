/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.io;

/**
	Helper that converts between floating point and binary representation.
	Always works in low-endian encoding.
**/
class FPHelper {
	static var i64tmp:Int64 = Int64.ofInt(0);

	#if !(lua_ver >= 5.3)
	static var hasStringPack:Bool = untyped __lua__("string.pack ~= nil");
	#end

	public static function i32ToFloat(i:Int):Float {
		#if (lua_ver >= 5.3)
		return untyped __lua__("string.unpack('<f', string.pack('<i4', {0}))", i);
		#else
		return hasStringPack ? untyped __lua__("string.unpack('<f', string.pack('<i4', {0}))", i) : _i32ToFloat(i);
		#end
	}

	public static function floatToI32(f:Float):Int {
		#if (lua_ver >= 5.3)
		return untyped __lua__("string.unpack('<i4', string.pack('<f', {0}))", f);
		#else
		return hasStringPack ? untyped __lua__("string.unpack('<i4', string.pack('<f', {0}))", f) : _floatToI32(f);
		#end
	}

	public static function i64ToDouble(low:Int, high:Int):Float {
		#if (lua_ver >= 5.3)
		return untyped __lua__("string.unpack('<d', string.pack('<i4i4', {0}, {1}))", low, high);
		#else
		return hasStringPack ? untyped __lua__("string.unpack('<d', string.pack('<i4i4', {0}, {1}))", low, high) : _i64ToDouble(low, high);
		#end
	}

	/**
		Returns an Int64 representing the bytes representation of the double precision IEEE float value.
		WARNING : for performance reason, the same Int64 value might be reused every time. Copy its low/high values before calling again.
		We still ensure that this is safe to use in a multithread environment.
	**/
	public static function doubleToI64(v:Float):Int64 {
		#if (lua_ver >= 5.3)
		return _doubleToI64_native(v);
		#else
		return hasStringPack ? _doubleToI64_native(v) : _doubleToI64_fallback(v);
		#end
	}

	static function _doubleToI64_native(v:Float):Int64 @:privateAccess {
		var low:Int = untyped __lua__("string.unpack('<i4', string.pack('<d', {0}))", v);
		var high:Int = untyped __lua__("string.unpack('<i4', string.pack('<d', {0}), 5)", v);
		i64tmp.set_low(low);
		i64tmp.set_high(high);
		return i64tmp;
	}

	#if !(lua_ver >= 5.3)
	static inline var LN2 = 0.6931471805599453;

	static function _doubleToI64_fallback(v:Float):Int64 @:privateAccess {
		var i64 = i64tmp;
		if (v == 0) {
			if (1.0 / v == Math.NEGATIVE_INFINITY) {
				i64.set_low(0);
				i64.set_high(untyped __lua__("0x80000000"));
			} else {
				i64.set_low(0);
				i64.set_high(0);
			}
		} else if (!Math.isFinite(v)) {
			if (Math.isNaN(v)) {
				i64.set_low(0);
				i64.set_high(untyped __lua__("0x7FF80000"));
			} else {
				i64.set_low(0);
				i64.set_high(v > 0 ? untyped __lua__("0x7FF00000") : untyped __lua__("0xFFF00000"));
			}
		} else {
			var av = v < 0 ? -v : v;
			var exp = Math.floor(Math.log(av) / LN2);
			if (exp > 1023) {
				i64.set_low(untyped __lua__("0xFFFFFFFF"));
				i64.set_high(untyped __lua__("0x7FEFFFFF"));
			} else {
				if (exp <= -1023) {
					exp = -1023;
					av = av / 2.2250738585072014e-308;
				} else {
					av = av / Math.pow(2, exp) - 1.0;
				}
				var sig = av * 4503599627370496.; // 2^52
				var sig_l:Int = untyped __lua__("math.floor({0} % 4294967296)", sig);
				var sig_h:Int = untyped __lua__("math.floor({0} / 4294967296)", sig);
				i64.set_low(sig_l);
				var signBit:Int = v < 0 ? untyped __lua__("0x80000000") : 0;
				var expBits:Int = (exp + 1023) << 20;
				var high_bits:Int = untyped __lua__("{0} + {1} + {2}", signBit, expBits, sig_h);
				i64.set_high(high_bits);
			}
		}
		return i64;
	}

	static function _i32ToFloat(i:Int):Float {
		var sign = 1 - ((i >>> 31) << 1);
		var e = (i >> 23) & 0xff;
		if (e == 255) {
			var m = i & 0x7fffff;
			return m == 0 ? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY) : Math.NaN;
		}
		var m = e == 0 ? (i & 0x7fffff) << 1 : (i & 0x7fffff) | 0x800000;
		return sign * m * Math.pow(2, e - 150);
	}

	static function _floatToI32(f:Float):Int {
		if (f == 0) {
			return 1.0 / f == Math.NEGATIVE_INFINITY ? untyped __lua__("0x80000000") : 0;
		}
		if (!Math.isFinite(f)) {
			if (Math.isNaN(f)) return untyped __lua__("0x7FC00000");
			return f > 0 ? untyped __lua__("0x7F800000") : untyped __lua__("0xFF800000");
		}
		var af = f < 0 ? -f : f;
		var exp = Math.floor(Math.log(af) / LN2);
		if (exp > 127) {
			return f < 0 ? untyped __lua__("0xFF800000") : untyped __lua__("0x7F800000");
		}
		var mantissa:Float;
		if (exp <= -127) {
			exp = -127;
			mantissa = af * 7.1362384635298e+44;
		} else {
			mantissa = (af / Math.pow(2, exp) - 1.0) * 0x800000;
		}
		var sign:Int = f < 0 ? untyped __lua__("0x80000000") : 0;
		var expBits:Int = (exp + 127) << 23;
		var mantissaBits:Int = Math.round(mantissa);
		return untyped __lua__("{0} + {1} + {2}", sign, expBits, mantissaBits);
	}

	static function _i64ToDouble(lo:Int, hi:Int):Float {
		var sign = 1 - ((hi >>> 31) << 1);
		var e = (hi >> 20) & 0x7ff;
		if (e == 2047) {
			var loIsZero = lo == 0;
			var hiMantissa = hi & 0xFFFFF;
			return loIsZero && hiMantissa == 0 ? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY) : Math.NaN;
		}
		var hiMantissa:Float = hi & 0xFFFFF;
		var loUnsigned:Float = lo < 0 ? lo + 4294967296.0 : lo;
		var m = 2.220446049250313e-16 * (hiMantissa * 4294967296. + loUnsigned);
		m = e == 0 ? m * 2.0 : m + 1.0;
		return sign * m * Math.pow(2, e - 1023);
	}
	#end
}
