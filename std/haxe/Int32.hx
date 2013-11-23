/*
 * Copyright (C)2005-2013 Haxe Foundation
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
 package haxe;

 /**
 	Int32 provides a 32-bit integer with consistent overflow behavior across
 	all platforms.
 **/
abstract Int32(Int) from Int to Int {
	@:op(-A) public function negate():Int32;

	@:op(++A) public inline function preIncrement():Int32
		return this = clamp(++this);

	@:op(A++) public inline function postIncrement():Int32 {
		var ret = this++;
		this = clamp(this);
		return ret;
	}

	@:op(--A) inline public function preDecrement():Int32
		return this = clamp(--this);

	@:op(A--) inline public function postDecrement():Int32 {
		var ret = this++;
		this = clamp(this);
		return ret;
	}

	@:op(A + B) inline public static function add(a:Int32, b:Int32):Int32
		return clamp( a + b );

	@:op(A + B) inline public static function addInt(a:Int32, b:Int):Int32
		return clamp( (a : Int) + b );

	@:op(A + B) public static function addFloat(a:Int32, b:Float):Float;

	@:op(A - B) inline public static function sub(a:Int32, b:Int32):Int32
		return clamp( a - b );

	@:op(A - B) public static function subInt(a:Int32, b:Int):Int32
		return clamp( (a : Int) - b );

	@:op(A - B) public static function subFloat(a:Int32, b:Int32):Float;

	#if (as3 || flash8 || js || php)

	@:op(A * B) inline public static function mul(a:Int32, b:Int32):Int32
		return clamp( a * (b & 0xFFFF) + clamp( a * (b >>> 16) << 16 ) );

	@:op(A * B) inline public static function mulInt(a:Int32, b:Int):Int32
		return mul(a, b);

	#else

	@:op(A * B) public static function mul(a:Int32, b:Int32):Int32;
	@:op(A * B) @:commutative public static function mulInt(a:Int32, b:Int):Int32;

	#end

	@:op(A * B) @:commutative public static function mulFloat(a:Int32, b:Float):Float;

	@:op(A / B) public static function div(a:Int32, b:Int32):Float;
	@:op(A / B) @:commutative public static function divInt(a:Int32, b:Int):Float;
	@:op(A / B) @:commutative public static function divFloat(a:Int32, b:Float):Float;

	@:op(A % B) public static function mod(a:Int32, b:Int32):Int32;
	@:op(A % B) @:commutative public static function modInt(a:Int32, b:Int):Int;
	@:op(A % B) @:commutative public static function modFloat(a:Int32, b:Float):Float;

	@:op(A == B) public static function eq(a:Int32, b:Int32):Bool;
	@:op(A == B) @:commutative public static function eqInt(a:Int32, b:Int):Bool;
	@:op(A == B) @:commutative public static function eqFloat(a:Int32, b:Float):Bool;

	@:op(A != B) public static function neq(a:Int32, b:Int32):Bool;
	@:op(A != B) @:commutative public static function neqInt(a:Int32, b:Int):Bool;
	@:op(A != B) @:commutative public static function neqFloat(a:Int32, b:Float):Bool;

	@:op(A < B) public static function lt(a:Int32, b:Int32):Bool;
	@:op(A < B) @:commutative public static function ltInt(a:Int32, b:Int):Bool;
	@:op(A < B) @:commutative public static function ltFloat(a:Int32, b:Float):Bool;

	@:op(A <= B) public static function lte(a:Int32, b:Int32):Bool;
	@:op(A <= B) @:commutative public static function lteInt(a:Int32, b:Int):Bool;
	@:op(A <= B) @:commutative public static function lteFloat(a:Int32, b:Float):Bool;

	@:op(A > B) public static function gt(a:Int32, b:Int32):Bool;
	@:op(A > B) @:commutative public static function gtInt(a:Int32, b:Int):Bool;
	@:op(A > B) @:commutative public static function gtFloat(a:Int32, b:Float):Bool;

	@:op(A >= B) public static function gte(a:Int32, b:Int32):Bool;
	@:op(A >= B) @:commutative public static function gteInt(a:Int32, b:Int):Bool;
	@:op(A >= B) @:commutative public static function gteFloat(a:Int32, b:Float):Bool;

	@:op(~A) public function complement():Int32;

	@:op(A & B) public static function and(a:Int32, b:Int32):Int32;
	@:op(A & B) @:commutative public static function andInt(a:Int32, b:Int):Int32;

	@:op(A | B) public static function or(a:Int32, b:Int32):Int32;
	@:op(A | B) @:commutative public static function orInt(a:Int32, b:Int):Int32;

	@:op(A ^ B) public static function xor(a:Int32, b:Int32):Int32;
	@:op(A ^ B) @:commutative public static function xorInt(a:Int32, b:Int):Int32;


	@:op(A >> B) public static function shr(a:Int32, b:Int32):Int32;
	@:op(A >> B) @:commutative public static function shrInt(a:Int32, b:Int):Int32;

	@:op(A >>> B) public static function ushr(a:Int32, b:Int32):Int32;
	@:op(A >>> B) @:commutative public static function ushrInt(a:Int32, b:Int):Int32;

	#if php

	// PHP may be 64-bit, so shifts must be clamped
	@:op(A << B) public static function shl(a:Int32, b:Int32):Int32
		return clamp( a << b );

	@:op(A << B) @:commutative public static function shlInt(a:Int32, b:Int):Int32
		return clamp( a << b );

	#else

	@:op(A << B) public static function shl(a:Int32, b:Int32):Int32;
	@:op(A << B) @:commutative public static function shlInt(a:Int32, b:Int):Int32;

	#end

	@:to public inline function toFloat():Float
		return this;

	#if php
	static var extraBits : Int = untyped __php__("PHP_INT_SIZE") * 8 - 32;
	#end

	inline static function clamp( x : Int ) : Int {
		// force to-int conversion on platforms that require it
		#if (as3 || flash8 || js)
		return x | 0;
		#elseif php
		// we might be on 64-bit php, so sign extend from 32-bit
		return (x << extraBits) >> extraBits;
		#else
		return (x);
		#end
	}
}
