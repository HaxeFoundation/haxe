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

package hl;

import haxe.Int64;

@:coreType @:notNull @:runtimeValue abstract I64 {
	/** The greatest representable I64 value. */
	static public var MAX(get,never):I64;
	@:hlNative("std", "num_i64_max") static function get_MAX():I64
		return 0;

	/** The smallest representable I64 value. */
	static public var MIN(get,never):I64;
	@:hlNative("std", "num_i64_min") static function get_MIN():I64
		return 0;

	@:hlNative("std", "num_i64_of_int")
	@:from public static function ofInt(i:Int):I64
		return cast 0;

	/**
		Destructively cast to Int
	**/
	public inline function toInt():Int {
		return cast this;
	}

	@:to
	@:deprecated("Implicit cast from I64 to Int (32 bits) is deprecated. Use .toInt() or explicitly cast instead.")
	inline function implicitToInt(): Int {
		return toInt();
	}

	/**
		Parse the given string value to I64.
	**/
	@:hlNative("std", "num_i64_of_string")
	static public function ofString(s:String):I64;

	/**
		Parse the given string value to I64.
	**/
	@:to public inline function toString():String {
		return @:privateAccess String.fromUTF8(__toString());
	}

	@:hlNative("std", "num_i64_to_bytes")
	function __toString():Bytes;

	/**
		Convert `haxe.Int64` to `hl.uv.I64`
	**/
	@:from static public function ofInt64(hx:Int64):I64 {
		return ((hx.high:I64) << 31) | (hx.low:I64);
	}

	/**
		Convert to `haxe.Int64`
	**/
	@:to public function toInt64():Int64 {
		return Int64.make((this >> 31).toInt(), this.toInt());
	}

	/**
		Integer division.
	**/
	@:hlNative("std", "num_i64_div")
	public function div(i:I64):I64
		return 0;

	@:op(A + B) @:hlNative("std", "num_i64_add") function add(u:I64):I64 return 0;
	@:op(A - B) @:hlNative("std", "num_i64_sub") function sub(u:I64):I64 return 0;
	@:op(A * B) @:hlNative("std", "num_i64_mul") function mul(u:I64):I64 return 0;
	@:op(A % B) @:hlNative("std", "num_i64_mod") function mod(u:I64):I64 return 0;
	@:op(A & B) @:hlNative("std", "num_i64_logand") function logand(u:I64):I64 return 0;
	@:op(A | B) @:hlNative("std", "num_i64_logor") function logor(u:I64):I64 return 0;
	@:op(A ^ B) @:hlNative("std", "num_i64_logxor") function logxor(u:I64):I64 return 0;
	@:op(A << B) @:hlNative("std", "num_i64_shift_left") function shift_left(i:Int):I64 return 0;
	@:op(A >> B) @:hlNative("std", "num_i64_shift_right") function shift_right(i:Int):I64 return 0;
	@:op(~A) @:hlNative("std", "num_i64_lognot") function lognot():I64 return 0;

	@:op(A != B) static function ne(a:I64, b:I64):Bool return !eq(a, b);
	@:op(A == B) @:hlNAtive("std", "num_i64_eq") static function eq(a:I64, b:I64):Bool return false;
	@:op(A < B) @:hlNAtive("std", "num_i64_lt") static function lt(a:I64, b:I64):Bool return false;
	@:op(A > B) @:hlNAtive("std", "num_i64_gt") static function gt(a:I64, b:I64):Bool return false;
	@:op(A <= B) @:hlNAtive("std", "num_i64_lte") static function lte(a:I64, b:I64):Bool return false;
	@:op(A >= B) @:hlNAtive("std", "num_i64_gte") static function gte(a:I64, b:I64):Bool return false;
}
