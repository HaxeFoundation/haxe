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
 package haxe;

 /**
 	Int32 provides a 32-bit integer with consistent overflow behavior across
 	all platforms.
 **/
abstract Int32(Int) from Int to Int {
	@:op(-A) private function negate():Int32;

	@:op(++A) private inline function preIncrement():Int32
		return this = clamp(++this);

	@:op(A++) private inline function postIncrement():Int32 {
		var ret = this++;
		this = clamp(this);
		return ret;
	}

	@:op(--A) private inline function preDecrement():Int32
		return this = clamp(--this);

	@:op(A--) private inline function postDecrement():Int32 {
		var ret = this--;
		this = clamp(this);
		return ret;
	}

	@:op(A + B) private static inline function add(a:Int32, b:Int32):Int32
		return clamp( (a : Int) + (b : Int) );

	@:op(A + B) @:commutative private static inline function addInt(a:Int32, b:Int):Int32
		return clamp( (a : Int) + (b : Int) );

	@:op(A + B) @:commutative private static function addFloat(a:Int32, b:Float):Float;

	@:op(A - B) private static inline function sub(a:Int32, b:Int32):Int32
		return clamp( (a : Int) - (b : Int) );

	@:op(A - B) private static inline function subInt(a:Int32, b:Int):Int32
		return clamp( (a : Int) - (b : Int) );

	@:op(A - B) private static inline function intSub(a:Int, b:Int32):Int32
		return clamp( (a : Int) - (b : Int) );

	@:op(A - B) private static function subFloat(a:Int32, b:Float):Float;

	@:op(A - B) public static function floatSub(a:Float, b:Int32):Float;

	#if (as3 || js || php || python || lua)

	#if js
	// on JS we want to try using Math.imul, but we have to assign that function to Int32.mul only once,
	// or else V8 will deoptimize it, so we need to be a bit funky with this.
	// See https://github.com/HaxeFoundation/haxe/issues/5367 for benchmarks.
	@:op(A * B) inline static function mul(a:Int32, b:Int32):Int32
		return _mul(a, b);

	static var _mul:Int32->Int32->Int32 = untyped
		if (Math.imul != null)
			Math.imul
		else
			function(a:Int32, b:Int32):Int32
				return clamp( (a : Int) * ((b : Int) & 0xFFFF) + clamp( (a : Int) * ((b : Int) >>> 16) << 16 ) );

	#else
	@:op(A * B) private static function mul(a:Int32, b:Int32):Int32
		return clamp( (a : Int) * ((b : Int) & 0xFFFF) + clamp( (a : Int) * ((b : Int) >>> 16) << 16 ) );
	#end

	@:op(A * B) @:commutative private static inline function mulInt(a:Int32, b:Int):Int32
		return mul(a, b);

	#else

	@:op(A * B) private static function mul(a:Int32, b:Int32):Int32;
	@:op(A * B) @:commutative private static function mulInt(a:Int32, b:Int):Int32;

	#end

	@:op(A * B) @:commutative private static function mulFloat(a:Int32, b:Float):Float;

	@:op(A / B) private static function div(a:Int32, b:Int32):Float;
	@:op(A / B) private static function divInt(a:Int32, b:Int):Float;
	@:op(A / B) private static function intDiv(a:Int, b:Int32):Float;
	@:op(A / B) private static function divFloat(a:Int32, b:Float):Float;
	@:op(A / B) private static function floatDiv(a:Float, b:Int32):Float;

	@:op(A % B) private static function mod(a:Int32, b:Int32):Int32;
	@:op(A % B) private static function modInt(a:Int32, b:Int):Int;
	@:op(A % B) private static function intMod(a:Int, b:Int32):Int;
	@:op(A % B) private static function modFloat(a:Int32, b:Float):Float;
	@:op(A % B) private static function floatMod(a:Float, b:Int32):Float;

	@:op(A == B) private static function eq(a:Int32, b:Int32):Bool;
	@:op(A == B) @:commutative private static function eqInt(a:Int32, b:Int):Bool;
	@:op(A == B) @:commutative private static function eqFloat(a:Int32, b:Float):Bool;

	@:op(A != B) private static function neq(a:Int32, b:Int32):Bool;
	@:op(A != B) @:commutative private static function neqInt(a:Int32, b:Int):Bool;
	@:op(A != B) @:commutative private static function neqFloat(a:Int32, b:Float):Bool;

	@:op(A < B) private static function lt(a:Int32, b:Int32):Bool;
	@:op(A < B) private static function ltInt(a:Int32, b:Int):Bool;
	@:op(A < B) private static function intLt(a:Int, b:Int32):Bool;
	@:op(A < B) private static function ltFloat(a:Int32, b:Float):Bool;
	@:op(A < B) private static function floatLt(a:Float, b:Int32):Bool;

	@:op(A <= B) private static function lte(a:Int32, b:Int32):Bool;
	@:op(A <= B) private static function lteInt(a:Int32, b:Int):Bool;
	@:op(A <= B) private static function intLte(a:Int, b:Int32):Bool;
	@:op(A <= B) private static function lteFloat(a:Int32, b:Float):Bool;
	@:op(A <= B) private static function floatLte(a:Float, b:Int32):Bool;

	@:op(A > B) private static function gt(a:Int32, b:Int32):Bool;
	@:op(A > B) private static function gtInt(a:Int32, b:Int):Bool;
	@:op(A > B) private static function intGt(a:Int, b:Int32):Bool;
	@:op(A > B) private static function gtFloat(a:Int32, b:Float):Bool;
	@:op(A > B) private static function floatGt(a:Float, b:Int32):Bool;

	@:op(A >= B) private static function gte(a:Int32, b:Int32):Bool;
	@:op(A >= B) private static function gteInt(a:Int32, b:Int):Bool;
	@:op(A >= B) private static function intGte(a:Int, b:Int32):Bool;
	@:op(A >= B) private static function gteFloat(a:Int32, b:Float):Bool;
	@:op(A >= B) private static function floatGte(a:Float, b:Int32):Bool;

	#if lua
	@:op(~A) private static inline function complement( a : Int32 ) : Int32
		return lua.Boot.clamp(~a);
	#else
	@:op(~A) private function complement():Int32;
	#end

	@:op(A & B) private static function and(a:Int32, b:Int32):Int32;
	@:op(A & B) @:commutative private static function andInt(a:Int32, b:Int):Int32;

	#if lua
	@:op(A | B) private static function or(a:Int32, b:Int32):Int32
		return clamp((a:Int) | (b:Int));
	@:op(A | B) @:commutative private static function orInt(a:Int32, b:Int):Int32
		return clamp((a:Int) | b);
	#else
	@:op(A | B) private static function or(a:Int32, b:Int32):Int32;
	@:op(A | B) @:commutative private static function orInt(a:Int32, b:Int):Int32;
	#end

	#if lua
	@:op(A ^ B) private static function xor(a:Int32, b:Int32):Int32
		return clamp((a:Int) ^ (b:Int));
	@:op(A ^ B) @:commutative private static function xorInt(a:Int32, b:Int):Int32
		return clamp((a:Int) ^ b);
	#else
	@:op(A ^ B) private static function xor(a:Int32, b:Int32):Int32;
	@:op(A ^ B) @:commutative private static function xorInt(a:Int32, b:Int):Int32;
	#end


#if lua
	@:op(A >> B) private static function shr(a:Int32, b:Int32):Int32
		return clamp((a:Int) >> (b:Int));
	@:op(A >> B) private static function shrInt(a:Int32, b:Int):Int32
		return clamp((a:Int) >> b);
	@:op(A >> B) private static function intShr(a:Int, b:Int32):Int32
		return clamp(a >> (b:Int));
#else
	@:op(A >> B) private static function shr(a:Int32, b:Int32):Int32;
	@:op(A >> B) private static function shrInt(a:Int32, b:Int):Int32;
	@:op(A >> B) private static function intShr(a:Int, b:Int32):Int32;
#end

	@:op(A >>> B) private static function ushr(a:Int32, b:Int32):Int32;
	@:op(A >>> B) private static function ushrInt(a:Int32, b:Int):Int32;
	@:op(A >>> B) private static function intUshr(a:Int, b:Int32):Int32;

	#if (php || python || lua)

	// PHP may be 64-bit, so shifts must be clamped
	@:op(A << B) private static inline function shl(a:Int32, b:Int32):Int32
		return clamp( (a : Int) << (b : Int) );

	@:op(A << B) private static inline function shlInt(a:Int32, b:Int):Int32
		return clamp( (a : Int) << b );

	@:op(A << B) private static inline function intShl(a:Int, b:Int32):Int32
 		return clamp( a << (b : Int) );

	#else

	@:op(A << B) private static function shl(a:Int32, b:Int32):Int32;
	@:op(A << B) private static function shlInt(a:Int32, b:Int):Int32;
	@:op(A << B) private static function intShl(a:Int, b:Int32):Int32;

	#end


	@:to private inline function toFloat():Float
		return this;

	/**
		Compare `a` and `b` in unsigned mode.
	**/
	public static function ucompare( a : Int32, b : Int32 ) : Int {
		if( a < 0 )
			return b < 0 ? ( ~b - ~a ) : 1;
		return b < 0 ? -1 : (a - b);
	}

	#if php
	static var extraBits : Int = untyped __php__("PHP_INT_SIZE") * 8 - 32;
	#end

#if !lua inline #end
	static function clamp( x : Int ) : Int {
		// force to-int conversion on platforms that require it
		#if (as3 || js)
		return x | 0;
		#elseif php
		// we might be on 64-bit php, so sign extend from 32-bit
		return (x << extraBits) >> extraBits;
		#elseif python
		return python.Syntax.pythonCode("{0} % {1}", (x + python.Syntax.opPow(2, 31)), python.Syntax.opPow(2, 32)) - python.Syntax.opPow(2, 31);
		#elseif lua
		return lua.Boot.clamp(x);
		#else
		return (x);
		#end
	}
}
