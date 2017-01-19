/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package java;

@:notNull @:runtimeValue @:coreType extern abstract Int8 from Int {}
@:notNull @:runtimeValue @:coreType extern abstract Int16 from Int {}
@:notNull @:runtimeValue @:coreType extern abstract Char16 from Int {}
@:notNull @:runtimeValue @:coreType extern abstract Int64 from Int from Float
{
	@:op(A+B) public static function addI(lhs:Int64, rhs:Int):Int64;
	@:op(A+B) public static function add(lhs:Int64, rhs:Int64):Int64;
	@:op(A*B) public static function mulI(lhs:Int64, rhs:Int):Int64;
	@:op(A*B) public static function mul(lhs:Int64, rhs:Int64):Int64;
	@:op(A%B) public static function modI(lhs:Int64, rhs:Int):Int64;
	@:op(A%B) public static function mod(lhs:Int64, rhs:Int64):Int64;
	@:op(A-B) public static function subI(lhs:Int64, rhs:Int):Int64;
	@:op(A-B) public static function sub(lhs:Int64, rhs:Int64):Int64;
	@:op(A/B) public static function divI(lhs:Int64, rhs:Int):Int64;
	@:op(A/B) public static function div(lhs:Int64, rhs:Int64):Int64;
	@:op(A|B) public static function orI(lhs:Int64, rhs:Int):Int64;
	@:op(A|B) public static function or(lhs:Int64, rhs:Int64):Int64;
	@:op(A^B) public static function xorI(lhs:Int64, rhs:Int):Int64;
	@:op(A^B) public static function xor(lhs:Int64, rhs:Int64):Int64;
	@:op(A&B) public static function andI(lhs:Int64, rhs:Int):Int64;
	@:op(A&B) public static function and(lhs:Int64, rhs:Int64):Int64;
	@:op(A<<B) public static function shlI(lhs:Int64, rhs:Int):Int64;
	@:op(A<<B) public static function shl(lhs:Int64, rhs:Int64):Int64;
	@:op(A>>B) public static function shrI(lhs:Int64, rhs:Int):Int64;
	@:op(A>>B) public static function shr(lhs:Int64, rhs:Int64):Int64;
	@:op(A>>>B) public static function ushrI(lhs:Int64, rhs:Int):Int64;
	@:op(A>>>B) public static function ushr(lhs:Int64, rhs:Int64):Int64;

	@:op(A>B) public static function gt(lhs:Int64, rhs:Int64):Bool;
	@:op(A>=B) public static function gte(lhs:Int64, rhs:Int64):Bool;
	@:op(A<B) public static function lt(lhs:Int64, rhs:Int64):Bool;
	@:op(A<=B) public static function lte(lhs:Int64, rhs:Int64):Bool;

	@:op(~A) public static function bneg(t:Int64):Int64;
	@:op(-A) public static function neg(t:Int64):Int64;

	@:op(++A) public static function preIncrement(t:Int64):Int64;
	@:op(A++) public static function postIncrement(t:Int64):Int64;
	@:op(--A) public static function preDecrement(t:Int64):Int64;
	@:op(A--) public static function postDecrement(t:Int64):Int64;
}
