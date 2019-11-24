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
	#if neko_v21
	// stored in helper
	#elseif neko
	static var i64tmp = new sys.thread.Tls<Int64>();
	#elseif !(java || cs || cpp)
	static var i64tmp = Int64.ofInt(0);

	static inline var LN2 = 0.6931471805599453; // Math.log(2)

	static inline function _i32ToFloat(i:Int):Float {
		var sign = 1 - ((i >>> 31) << 1);
		var e = (i >> 23) & 0xff;
		if (e == 255)
			return i & 0x7fffff == 0 ? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY) : Math.NaN;
		var m = e == 0 ? (i & 0x7fffff) << 1 : (i & 0x7fffff) | 0x800000;
		return sign * m * Math.pow(2, e - 150);
	}

	static inline function _i64ToDouble(lo:Int, hi:Int):Float {
		var sign = 1 - ((hi >>> 31) << 1);
		var e = (hi >> 20) & 0x7ff;
		if (e == 2047)
			return lo == 0 && (hi & 0xFFFFF) == 0 ? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY) : Math.NaN;
		var m = 2.220446049250313e-16 * ((hi & 0xFFFFF) * 4294967296. + (lo >>> 31) * 2147483648. + (lo & 0x7FFFFFFF));
		m = e == 0 ? m * 2.0 : m + 1.0;
		return sign * m * Math.pow(2, e - 1023);
	}

	static inline function _floatToI32(f:Float):Int {
		if (f == 0)
			return 0;
		var af = f < 0 ? -f : f;
		var exp = Math.floor(Math.log(af) / LN2);
		if (exp > 127) {
			return 0x7F800000;
		} else {
			if (exp <= -127) {
				exp = -127;
				af *= 7.1362384635298e+44; // af * 0.5 * 0x800000 / Math.pow(2, -127)
			} else {
				af = (af / Math.pow(2, exp) - 1.0) * 0x800000;
			}
			return (f < 0 ? 0x80000000 : 0) | ((exp + 127) << 23) | Math.round(af);
		}
	}

	static inline function _doubleToI64(v:Float):Int64@:privateAccess {
		var i64 = i64tmp;
		if (v == 0) {
			i64.set_low(0);
			i64.set_high(0);
		} else if (!Math.isFinite(v)) {
			i64.set_low(0);
			i64.set_high(v > 0 ? 0x7FF00000 : 0xFFF00000);
		} else {
			var av = v < 0 ? -v : v;
			var exp = Math.floor(Math.log(av) / LN2);
			if (exp > 1023) {
				i64.set_low(0xFFFFFFFF);
				i64.set_high(0x7FEFFFFF);
			} else {
				if (exp <= -1023) {
					exp = -1023;
					av = av / 2.2250738585072014e-308;
				} else {
					av = av / Math.pow(2, exp) - 1.0;
				}
				var sig = Math.fround(av * 4503599627370496.); // 2^52
				// Note: If "sig" is outside of the signed Int32 range, the result is unspecified in HL, C#, Java and Neko,
				var sig_l = Std.int(sig);
				var sig_h = Std.int(sig / 4294967296.0);
				i64.set_low(sig_l);
				i64.set_high((v < 0 ? 0x80000000 : 0) | ((exp + 1023) << 20) | sig_h);
			}
		}
		return i64;
	}
	#end

	#if neko
	#if neko_v21
	static var helpers = new sys.thread.Tls<neko.NativeArray<Dynamic>>();
	#else
	static var helperf = new sys.thread.Tls<neko.NativeString>();
	static var helperd = new sys.thread.Tls<neko.NativeString>();
	static var _float_of_bytes = neko.Lib.load("std", "float_of_bytes", 2);
	static var _double_of_bytes = neko.Lib.load("std", "double_of_bytes", 2);
	static var _float_bytes = neko.Lib.load("std", "float_bytes", 2);
	static var _double_bytes = neko.Lib.load("std", "double_bytes", 2);
	#end
	#elseif flash
	static var helper = {
		var b = new flash.utils.ByteArray();
		b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		b;
	}
	#elseif js
	static var helper = new js.lib.DataView(new js.lib.ArrayBuffer(8));
	#end

	#if neko_v21
	inline
	#end
	public static function i32ToFloat(i:Int):Float {
		#if neko
		#if neko_v21
		return untyped $itof(i, false);
		#else
		var helper = helperf.value;
		if (helper == null)
			helperf.value = helper = neko.NativeString.alloc(4);
		untyped $sset(helper, 0, i & 0xFF);
		untyped $sset(helper, 1, (i >> 8) & 0xFF);
		untyped $sset(helper, 2, (i >> 16) & 0xFF);
		untyped $sset(helper, 3, i >>> 24);
		return _float_of_bytes(helper, false);
		#end
		#elseif cpp
		return untyped __global__.__hxcpp_reinterpret_le_int32_as_float32(i);
		#elseif cs
		var helper = new SingleHelper(0);
		if (cs.system.BitConverter.IsLittleEndian) {
			helper.i = i;
		} else {
			helper.i = ((i >>> 24) & 0xFF) | (((i >> 16) & 0xFF) << 8) | (((i >> 8) & 0xFF) << 16) | ((i & 0xFF) << 24);
		}

		return helper.f;
		#elseif java
		return java.lang.Float.FloatClass.intBitsToFloat(i);
		#elseif flash
		var helper = helper;
		helper.position = 0;
		helper.writeUnsignedInt(i);
		helper.position = 0;
		return helper.readFloat();
		#elseif js
		helper.setInt32(0, i, true);
		return helper.getFloat32(0, true);
		#else
		return _i32ToFloat(i);
		#end
	}

	#if neko_v21
	inline
	#end
	public static function floatToI32(f:Float):Int {
		#if neko
		#if neko_v21
		return untyped $ftoi(f, false);
		#else
		var r = _float_bytes(f, false);
		return untyped $sget(r, 0) | ($sget(r, 1) << 8) | ($sget(r, 2) << 16) | ($sget(r, 3) << 24);
		#end
		#elseif cpp
		return untyped __global__.__hxcpp_reinterpret_float32_as_le_int32(f);
		#elseif cs
		var helper = new SingleHelper(f);
		if (cs.system.BitConverter.IsLittleEndian) {
			return helper.i;
		} else {
			var i = helper.i;
			return ((i >>> 24) & 0xFF) | (((i >> 16) & 0xFF) << 8) | (((i >> 8) & 0xFF) << 16) | ((i & 0xFF) << 24);
		}
		#elseif java
		return java.lang.Float.FloatClass.floatToRawIntBits(f);
		#elseif flash
		var helper = helper;
		helper.position = 0;
		helper.writeFloat(f);
		helper.position = 0;
		return helper.readUnsignedInt();
		#elseif js
		helper.setFloat32(0, f, true);
		return helper.getInt32(0, true);
		#else
		return _floatToI32(f);
		#end
	}

	#if neko_v21
	inline
	#end
	public static function i64ToDouble(low:Int, high:Int):Float {
		#if neko
		#if neko_v21
		return untyped $itod(low, high, false);
		#else
		var helper = helperd.value;
		if (helper == null)
			helperd.value = helper = neko.NativeString.alloc(8);
		untyped $sset(helper, 0, low & 0xFF);
		untyped $sset(helper, 1, (low >> 8) & 0xFF);
		untyped $sset(helper, 2, (low >> 16) & 0xFF);
		untyped $sset(helper, 3, low >>> 24);
		untyped $sset(helper, 4, high & 0xFF);
		untyped $sset(helper, 5, (high >> 8) & 0xFF);
		untyped $sset(helper, 6, (high >> 16) & 0xFF);
		untyped $sset(helper, 7, high >>> 24);
		return _double_of_bytes(helper, false);
		#end
		#elseif cpp
		return untyped __global__.__hxcpp_reinterpret_le_int32s_as_float64(low, high);
		#elseif cs
		var helper = new FloatHelper(0);
		if (cs.system.BitConverter.IsLittleEndian) {
			helper.i = haxe.Int64.make(high, low);
		} else {
			var i1 = high, i2 = low;
			var j2 = ((i1 >>> 24) & 0xFF) | (((i1 >> 16) & 0xFF) << 8) | (((i1 >> 8) & 0xFF) << 16) | ((i1 & 0xFF) << 24);
			var j1 = ((i2 >>> 24) & 0xFF) | (((i2 >> 16) & 0xFF) << 8) | (((i2 >> 8) & 0xFF) << 16) | ((i2 & 0xFF) << 24);
			helper.i = haxe.Int64.make(j1, j2);
		}
		return helper.f;
		#elseif java
		return java.lang.Double.DoubleClass.longBitsToDouble(Int64.make(high, low));
		#elseif flash
		var helper = helper;
		helper.position = 0;
		helper.writeUnsignedInt(low);
		helper.writeUnsignedInt(high);
		helper.position = 0;
		return helper.readDouble();
		#elseif js
		helper.setInt32(0, low, true);
		helper.setInt32(4, high, true);
		return helper.getFloat64(0, true);
		#else
		return _i64ToDouble(low, high);
		#end
	}

	/**
		Returns an Int64 representing the bytes representation of the double precision IEEE float value.
		WARNING : for performance reason, the same Int64 value might be reused every time. Copy its low/high values before calling again.
		We still ensure that this is safe to use in a multithread environment
	**/
	public static function doubleToI64(v:Float):Int64 {
		#if neko
		#if neko_v21
		var helper = helpers.value;
		if (helper == null) {
			helpers.value = helper = neko.NativeArray.alloc(2);
			helper[0] = neko.NativeArray.alloc(2);
			helper[1] = haxe.Int64.ofInt(0);
		}
		var i64:haxe.Int64 = helper[1], int2 = helper[0];
		untyped $dtoi(v, int2, false);
		@:privateAccess {
			i64.set_low(int2[0]);
			i64.set_high(int2[1]);
		}
		return i64;
		#else
		var r = _double_bytes(v, false), i64 = i64tmp.value;
		if (i64 == null)
			i64 = i64tmp.value = haxe.Int64.ofInt(0);
		@:privateAccess {
			i64.set_low(untyped $sget(r, 0) | ($sget(r, 1) << 8) | ($sget(r, 2) << 16) | ($sget(r, 3) << 24));
			i64.set_high(untyped $sget(r, 4) | ($sget(r, 5) << 8) | ($sget(r, 6) << 16) | ($sget(r, 7) << 24));
		}
		return i64;
		#end
		#elseif cpp
		return Int64.make(untyped __global__.__hxcpp_reinterpret_float64_as_le_int32_high(v),
			untyped __global__.__hxcpp_reinterpret_float64_as_le_int32_low(v));
		#elseif java
		return java.lang.Double.DoubleClass.doubleToRawLongBits(v);
		#elseif cs
		var helper = new FloatHelper(v);
		if (cs.system.BitConverter.IsLittleEndian) {
			return helper.i;
		} else {
			var i = helper.i;
			var i1 = haxe.Int64.getHigh(i), i2 = haxe.Int64.getLow(i);
			var j2 = ((i1 >>> 24) & 0xFF) | (((i1 >> 16) & 0xFF) << 8) | (((i1 >> 8) & 0xFF) << 16) | ((i1 & 0xFF) << 24);
			var j1 = ((i2 >>> 24) & 0xFF) | (((i2 >> 16) & 0xFF) << 8) | (((i2 >> 8) & 0xFF) << 16) | ((i2 & 0xFF) << 24);

			return haxe.Int64.make(j1, j2);
		}
		#elseif flash
		var helper = helper;
		helper.position = 0;
		helper.writeDouble(v);
		helper.position = 0;
		var i64 = i64tmp;
		@:privateAccess {
			i64.set_low(cast helper.readUnsignedInt());
			i64.set_high(cast helper.readUnsignedInt());
		}
		return i64;
		#elseif js
		var i64 = i64tmp;
		helper.setFloat64(0, v, true);
		@:privateAccess {
			i64.set_low(helper.getInt32(0, true));
			i64.set_high(helper.getInt32(4, true));
		}
		return i64;
		#else
		return _doubleToI64(v);
		#end
	}
}

#if cs
@:meta(System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit))
@:nativeGen @:struct private class SingleHelper {
	@:meta(System.Runtime.InteropServices.FieldOffset(0))
	public var i:Int;
	@:meta(System.Runtime.InteropServices.FieldOffset(0))
	public var f:Single;

	public function new(f:Single) {
		this.i = 0;
		this.f = f;
	}
}

@:meta(System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit))
@:nativeGen @:struct private class FloatHelper {
	@:meta(System.Runtime.InteropServices.FieldOffset(0))
	public var i:haxe.Int64;
	@:meta(System.Runtime.InteropServices.FieldOffset(0))
	public var f:Float;

	public function new(f:Float) {
		this.i = haxe.Int64.ofInt(0);
		this.f = f;
	}
}
#end
