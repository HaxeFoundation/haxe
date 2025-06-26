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

@:coreType @:notNull @:runtimeValue abstract I64 from Int {

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

	#if (hl_ver >= version("1.12.0") && !hl_legacy32)
	@:op(a+b) function add(v:I64) : I64;
	@:op(a-b) function sub(v:I64) : I64;
	@:op(a*b) function mul(v:I64) : I64;
	@:op(a/b) function div(v:I64) : I64;
	@:op(a%b) function mod(v:I64) : I64;
	@:op(a<<b) function shl(v:Int) : I64;
	@:op(a>>b) function shr(v:Int) : I64;
	@:op(a>>>b) function ushr(v:Int) : I64;
	@:op(a|b) function or(v:I64) : I64;
	@:op(a&b) function and(v:I64) : I64;
	@:op(a^b) function xor(v:I64) : I64;

	@:op(-a) function neg() : I64;
	@:op(~a) inline function compl() : I64 { return (-1:I64) - this; }
	@:op(++a) function incr() : I64;
	@:op(--a) function decr() : I64;
	@:op(a++) function pincr() : I64;
	@:op(a--) function pdecr() : I64;

	@:op(a==b) function eq(v:I64) : Bool;
	@:op(a>=b) function gte(v:I64) : Bool;
	@:op(a<=b) function lte(v:I64) : Bool;
	@:op(a>b) function gt(v:I64) : Bool;
	@:op(a<b) function lt(v:I64) : Bool;
	#end

}
