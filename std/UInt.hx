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
#if ((flash || flash9doc) && !doc_gen)
/**
The unsigned `Int` type is only defined for Flash. It's currently
handled the same as a normal Int.

@see https://haxe.org/manual/types-basic-types.html
**/
@:coreType
@:notNull
@:runtimeValue
@:analyzer(no_const_propagation)
abstract UInt to Int from Int {
@:commutative @:op(A + B) private static function addI(lhs:UInt, rhs:Int):UInt;

@:commutative @:op(A + B) private static function addF(lhs:UInt, rhs:Float):Float;

@:op(A + B) private static function add(lhs:UInt, rhs:UInt):UInt;

@:commutative @:op(A * B) private static function mulI(lhs:UInt, rhs:Int):UInt;

@:commutative @:op(A * B) private static function mulF(lhs:UInt, rhs:Float):Float;

@:op(A * B) private static function mul(lhs:UInt, rhs:UInt):UInt;

@:op(A % B) private static function modI(lhs:UInt, rhs:Int):UInt;

@:op(A % B) private static function modF(lhs:UInt, rhs:Float):Float;

@:op(A % B) private static function mod(lhs:UInt, rhs:UInt):UInt;

@:op(A - B) private static function subI(lhs:UInt, rhs:Int):UInt;

@:op(A - B) private static function subF(lhs:UInt, rhs:Float):Float;

@:op(A - B) private static function sub(lhs:UInt, rhs:UInt):UInt;

@:op(A / B) private static function divI(lhs:UInt, rhs:Int):Float;

@:op(A / B) private static function divF(lhs:UInt, rhs:Float):Float;

@:op(A / B) private static function div(lhs:UInt, rhs:UInt):Float;

@:commutative @:op(A | B) private static function orI(lhs:UInt, rhs:Int):UInt;

@:op(A | B) private static function or(lhs:UInt, rhs:UInt):UInt;

@:commutative @:op(A ^ B) private static function xorI(lhs:UInt, rhs:Int):UInt;

@:op(A ^ B) private static function xor(lhs:UInt, rhs:UInt):UInt;

@:commutative @:op(A & B) private static function andI(lhs:UInt, rhs:Int):UInt;

@:op(A & B) private static function and(lhs:UInt, rhs:UInt):UInt;

@:op(A << B) private static function shl(lhs:UInt, rhs:Int):UInt;

@:op(A >> B) private static inline function shr(lhs:UInt, rhs:Int):UInt
return lhs >>> rhs;

@:op(A >>> B) private static function ushr(lhs:UInt, rhs:Int):UInt;

@:op(A > B) private static function gt(lhs:UInt, rhs:UInt):Bool;

@:op(A >= B) private static function gte(lhs:UInt, rhs:UInt):Bool;

@:op(A < B) private static function lt(lhs:UInt, rhs:UInt):Bool;

@:op(A <= B) private static function lte(lhs:UInt, rhs:UInt):Bool;

@:op(A > B) private static function gtf(lhs:UInt, rhs:Float):Bool;

@:op(A > B) private static function gtf2(lhs:Float, rhs:UInt):Bool;

@:op(A >= B) private static function gtef(lhs:UInt, rhs:Float):Bool;

@:op(A >= B) private static function gtef2(lhs:Float, rhs:UInt):Bool;

@:op(A < B) private static function ltf(lhs:UInt, rhs:Float):Bool;

@:op(A < B) private static function ltf2(lhs:Float, rhs:UInt):Bool;

@:op(A <= B) private static function ltef(lhs:UInt, rhs:Float):Bool;

@:op(A <= B) private static function ltef2(lhs:Float, rhs:UInt):Bool;

@:op(~A) private static function bneg(t:UInt):UInt;

@:commutative @:op(A == B) private static function equalsInt<T:Int>(a:UInt, b:T):Bool;

@:commutative @:op(A != B) private static function notEqualsInt<T:Int>(a:UInt, b:T):Bool;

@:commutative @:op(A == B) private static function equalsFloat<T:Float>(a:UInt, b:T):Bool;

@:commutative @:op(A != B) private static function notEqualsFloat<T:Float>(a:UInt, b:T):Bool;

@:op(++A) private function prefixIncrement():UInt;

@:op(A++) private function postfixIncrement():UInt;

@:op(--A) private function prefixDecrement():UInt;

@:op(A--) private function postfixDecrement():UInt;
}
#else
/**
A cross-platform unsigned 32-bit integer.

@see https://haxe.org/manual/types-basic-types.html
@deprecated Use `haxe.UInt32` instead.
**/
@:deprecated("UInt is deprecated, use haxe.UInt32 instead")
typedef UInt = haxe.UInt32;
#end
