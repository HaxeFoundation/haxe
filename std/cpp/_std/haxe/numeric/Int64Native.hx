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

package haxe.numeric;

typedef Int64Native = Int64NativeImpl;

#if !cppia
@:include("cpp/Int64.h")
private extern class CppInt64Helper {
	@:native("_hx_int64_make")
	static function make(high:haxe.Int32, low:haxe.Int32):cpp.Int64;

	@:native("_hx_int64_is_neg")
	static function isNeg(a:cpp.Int64):Bool;

	@:native("_hx_int64_is_zero")
	static function isZero(a:cpp.Int64):Bool;

	@:native("_hx_int64_compare")
	static function compare(a:cpp.Int64, b:cpp.Int64):Int;

	@:native("_hx_int64_ucompare")
	static function ucompare(a:cpp.Int64, b:cpp.Int64):Int;

	@:native("_hx_int64_to_string")
	static function toString(a:cpp.Int64):String;

	@:native("_hx_int64_neg")
	static function neg(a:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_add")
	static function add(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_sub")
	static function sub(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_mul")
	static function mul(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_div")
	static function div(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_mod")
	static function mod(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_eq")
	static function eq(a:cpp.Int64, b:cpp.Int64):Bool;

	@:native("_hx_int64_neq")
	static function neq(a:cpp.Int64, b:cpp.Int64):Bool;

	@:native("_hx_int64_complement")
	static function complement(a:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_and")
	static function bitAnd(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_or")
	static function bitOr(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_xor")
	static function bitXor(a:cpp.Int64, b:cpp.Int64):cpp.Int64;

	@:native("_hx_int64_shl")
	static function shl(a:cpp.Int64, b:Int):cpp.Int64;

	@:native("_hx_int64_shr")
	static function shr(a:cpp.Int64, b:Int):cpp.Int64;

	@:native("_hx_int64_ushr")
	static function ushr(a:cpp.Int64, b:Int):cpp.Int64;

	@:native("_hx_int64_high")
	static function high(a:cpp.Int64):haxe.Int32;

	@:native("_hx_int64_low")
	static function low(a:cpp.Int64):haxe.Int32;
}
#end

#if cppia
extern
#end
private abstract Int64NativeImpl(cpp.Int64) from cpp.Int64 to cpp.Int64 {
	#if cppia
	public var high(get, never):haxe.Int32;
	public function get_high():haxe.Int32;

	public var low(get, never):haxe.Int32;
	public function get_low():haxe.Int32;

	public function new(high:haxe.Int32, low:haxe.Int32):Void;

	public static function make(high:haxe.Int32, low:haxe.Int32):Int64Native;
	public static function ofInt(x:Int):Int64Native;
	public static function toInt(x:Int64Native):Int;
	public static function isInt64(val:Dynamic):Bool;
	public static function isNeg(x:Int64Native):Bool;
	public static function isZero(x:Int64Native):Bool;
	public static function compare(a:Int64Native, b:Int64Native):Int;
	public static function ucompare(a:Int64Native, b:Int64Native):Int;
	@:op(-A) public static function neg(x:Int64Native):Int64Native;
	@:op(A + B) public static function add(a:Int64Native, b:Int64Native):Int64Native;
	@:op(A - B) public static function sub(a:Int64Native, b:Int64Native):Int64Native;
	@:op(A * B) public static function mul(a:Int64Native, b:Int64Native):Int64Native;
	public static function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native};
	public static function udivMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native};
	public static function utoString(x:Int64Native):String;
	public static function uparseString(sParam:String):Int64Native;
	public static function ufromFloat(f:Float):Int64Native;
	public static function utoFloat(x:Int64Native):Float;
	@:op(A == B) public static function eq(a:Int64Native, b:Int64Native):Bool;
	@:op(A != B) public static function neq(a:Int64Native, b:Int64Native):Bool;
	@:op(~A) public static function complement(x:Int64Native):Int64Native;
	@:op(A & B) public static function and(a:Int64Native, b:Int64Native):Int64Native;
	@:op(A | B) public static function or(a:Int64Native, b:Int64Native):Int64Native;
	@:op(A ^ B) public static function xor(a:Int64Native, b:Int64Native):Int64Native;
	@:op(A << B) public static function shl(a:Int64Native, b:Int):Int64Native;
	@:op(A >> B) public static function shr(a:Int64Native, b:Int):Int64Native;
	@:op(A >>> B) public static function ushr(a:Int64Native, b:Int):Int64Native;
	public function toString():String;
	public static function parseString(sParam:String):Int64Native;
	public static function toFloat(x:Int64Native):Float;
	public static function fromFloat(f:Float):Int64Native;
	#else
	public var high(get, never):haxe.Int32;

	#if !scriptable inline #end function get_high():haxe.Int32
		return CppInt64Helper.high(this);

	public var low(get, never):haxe.Int32;

	#if !scriptable inline #end function get_low():haxe.Int32
		return CppInt64Helper.low(this);

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this = CppInt64Helper.make(high, low);
	}

	public static #if !scriptable inline #end function make(high:haxe.Int32, low:haxe.Int32):Int64Native {
		return new Int64Native(high, low);
	}

	public static #if !scriptable inline #end function ofInt(x:Int):Int64Native {
		return cast x;
	}

	public static #if !scriptable inline #end function toInt(x:Int64Native):Int {
		return x.low;
	}

	public static #if !scriptable inline #end function isInt64(val:Dynamic):Bool
		return val is cpp.Int64;

	public static #if !scriptable inline #end function isNeg(x:Int64Native):Bool
		return CppInt64Helper.isNeg(x);

	public static #if !scriptable inline #end function isZero(x:Int64Native):Bool
		return CppInt64Helper.isZero(x);

	public static #if !scriptable inline #end function compare(a:Int64Native, b:Int64Native):Int
		return CppInt64Helper.compare(a, b);

	public static #if !scriptable inline #end function ucompare(a:Int64Native, b:Int64Native):Int
		return CppInt64Helper.ucompare(a, b);

	@:op(-A) public static #if !scriptable inline #end function neg(x:Int64Native):Int64Native
		return CppInt64Helper.neg(x);

	@:op(A + B) public static #if !scriptable inline #end function add(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.add(a, b);

	@:op(A - B) public static #if !scriptable inline #end function sub(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.sub(a, b);

	@:op(A * B) public static #if !scriptable inline #end function mul(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.mul(a, b);

	public static function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native} {
		if (CppInt64Helper.isZero(divisor))
			throw "divide by zero";
		return {quotient: CppInt64Helper.div(dividend, divisor), modulus: CppInt64Helper.mod(dividend, divisor)};
	}

	@:op(A == B) public static #if !scriptable inline #end function eq(a:Int64Native, b:Int64Native):Bool
		return CppInt64Helper.eq(a, b);

	@:op(A != B) public static #if !scriptable inline #end function neq(a:Int64Native, b:Int64Native):Bool
		return CppInt64Helper.neq(a, b);

	@:op(~A) public static #if !scriptable inline #end function complement(x:Int64Native):Int64Native
		return CppInt64Helper.complement(x);

	@:op(A & B) public static #if !scriptable inline #end function and(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.bitAnd(a, b);

	@:op(A | B) public static #if !scriptable inline #end function or(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.bitOr(a, b);

	@:op(A ^ B) public static #if !scriptable inline #end function xor(a:Int64Native, b:Int64Native):Int64Native
		return CppInt64Helper.bitXor(a, b);

	@:op(A << B) public static #if !scriptable inline #end function shl(a:Int64Native, b:Int):Int64Native
		return CppInt64Helper.shl(a, b);

	@:op(A >> B) public static #if !scriptable inline #end function shr(a:Int64Native, b:Int):Int64Native
		return CppInt64Helper.shr(a, b);

	@:op(A >>> B) public static #if !scriptable inline #end function ushr(a:Int64Native, b:Int):Int64Native
		return CppInt64Helper.ushr(a, b);

	public #if !scriptable inline #end function toString():String
		return cast this;

	public static inline function parseString(sParam:String):Int64Native {
		return haxe.numeric.Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64Native {
		return haxe.numeric.Int64Helper.fromFloat(f);
	}

	public static #if !scriptable inline #end function toFloat(x:Int64Native):Float {
		var f:Float = x.low;
		if (f < 0)
			f += 4294967296.0;
		return (x.high : Float) * 4294967296.0 + f;
	}

	public static function udivMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native} {
		return haxe.numeric.UInt64Helper.udivMod(dividend, divisor);
	}

	public static function utoString(x:Int64Native):String {
		return haxe.numeric.UInt64Helper.utoString(x);
	}

	public static function uparseString(sParam:String):Int64Native {
		return haxe.numeric.UInt64Helper.parseString(sParam);
	}

	public static function ufromFloat(f:Float):Int64Native {
		return haxe.numeric.UInt64Helper.fromFloat(f);
	}

	public static function utoFloat(x:Int64Native):Float {
		return haxe.numeric.UInt64Helper.toFloat(x);
	}
	#end
}
