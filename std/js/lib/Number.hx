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
package js.lib;

import js.Syntax;
import js.lib.intl.NumberFormat.NumberFormatOptions;

/**
	The `Number` JavaScript object is a wrapper object allowing you to work with numerical values.

	Documentation [Number](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
 */
@:forward
abstract Number(_Number) {
	// Cast
	@:from static inline function from(x:Float):Number {
		return _Number.fromFloat(x);
	}
	
	@:to inline function toFloat():Float {
		return this.toFloat();
	}

	@:to inline function toInt():Int {
		return this.toInt();
	}

	// Operator overloading : Unary
	@:op(~A) inline function bitwiseMegation():Number {
		return ~(this.toInt());
	}

	@:op(-A) inline function minus():Number {
		return -(this.toFloat());
	}

	@:op(++A) inline function prefixIncrement():Number {
		return this.prefixIncrement();
	}

	@:op(A++) inline function postfixIncrement():Number {
		return this.postfixIncrement();
	}

	@:op(--A) inline function prefixDecrement():Number {
		return this.prefixDecrement();
	}

	@:op(A--) inline function postfixDecrement():Number {
		return this.postfixDecrement();
	}

	// Operator overloading : Arithmetic
	@:op(A % B) static inline function mod(a:Number, b:Number):Number {
		return (a:Float) % (b:Float);
	}
	@:op(A % B) static inline function modFloat(a:Number, b:Float):Number {
		return (a:Float) % b;
	}
	@:op(A % B) static inline function floatMod(a:Float, b:Number):Number {
		return a % (b:Float);
	}

	@:op(A * B) static inline function mul(a:Number, b:Number):Number {
		return (a:Float) * (b:Float);
	}
	@:op(A * B) @:commutative static inline function mulWithFloat(a:Number, b:Float):Number {
		return (a:Float) * b;
	}

	@:op(A / B) static inline function div(a:Number, b:Number):Number {
		return (a:Float) / (b:Float);
	}
	@:op(A / B) static inline function divFloat(a:Number, b:Float):Number {
		return (a:Float) / b;
	}
	@:op(A / B) static inline function floatDiv(a:Float, b:Number):Number {
		return a / (b:Float);
	}

	@:op(A + B) static inline function add(a:Number, b:Number):Number {
		return (a:Float) + (b:Float);
	}
	@:op(A + B) @:commutative static inline function addWidthFloat(a:Number, b:Float):Number {
		return (a:Float) + b;
	}

	@:op(A - B) static inline function sub(a:Number, b:Number):Number {
		return (a:Float) - (b:Float);
	}
	@:op(A - B) static inline function subFloat(a:Number, b:Float):Number {
		return (a:Float) - b;
	}
	@:op(A - B) static inline function floatSub(a:Float, b:Number):Number {
		return a - (b:Float);
	}

	// Operator overloading : Bitwise
	@:op(A << B) static inline function shiftLeft(a:Number, b:Int):Number {
		return (a:Int) << b;
	}

	@:op(A >> B) static inline function shiftRight(a:Number, b:Int):Number {
		return (a:Int) >> b;
	}

	@:op(A >>> B) static inline function unsignedShiftRight(a:Number, b:Int):Number {
		return (a:Int) >>> b;
	}

	@:op(A & B) static inline function and(a:Number, b:Number):Number {
		return (a:Int) & (b:Int);
	}
	@:op(A & B) static inline function andWithInt(a:Number, b:Int):Number {
		return (a:Int) & b;
	}

	@:op(A | B) static inline function or(a:Number, b:Number):Number {
		return (a:Int) | (b:Int);
	}
	@:op(A | B) static inline function orWithInt(a:Number, b:Int):Number {
		return (a:Int) | b;
	}

	@:op(A ^ B) static inline function xor(a:Number, b:Number):Number {
		return (a:Int) ^ (b:Int);
	}
	@:op(A ^ B) static inline function xorWithInt(a:Number, b:Int):Number {
		return (a:Int) ^ b;
	}

	// Operator overloading : Comparison
	@:op(A == B) @:commutative static inline function eqWithFloat(a:Number, b:Float):Bool {
		return (a:Float) == b;
	}

	@:op(A != B) @:commutative static inline function neqWithFloat(a:Number, b:Float):Bool {
		return (a:Float) != b;
	}

	@:op(A < B) static inline function ltFloat(a:Number, b:Float):Bool {
		return (a:Float) < b;
	}
	@:op(A < B) static inline function floatLt(a:Float, b:Number):Bool {
		return a < (b:Float);
	}

	@:op(A <= B) static inline function lteFloat(a:Number, b:Float) {
		return (a:Float) <= b;
	}
	@:op(A <= B) static inline function floatLte(a:Float, b:Number) {
		return a <= (b:Float);
	}

	@:op(A > B) static inline function gtFloat(a:Number, b:Float):Bool {
		return (a:Float) > b;
	}
	@:op(A > B) static inline function floatGt(a:Float, b:Number):Bool {
		return a > (b:Float);
	}

	@:op(A >= B) static inline function gteFloat(a:Number, b:Float) {
		return (a:Float) >= b;
	}
	@:op(A >= B) static inline function floatGte(a:Float, b:Number) {
		return a >= (b:Float);
	}

	// Static
	/**
		The smallest interval between two representable numbers.
	**/
	public static var EPSILON(get, never):Float;
	inline static function get_EPSILON() return _Number.EPSILON;

	/**
		The maximum safe integer in JavaScript `(2^53 - 1)`.
	**/
	public static var MAX_SAFE_INTEGER(get, never):Int;
	inline static function get_MAX_SAFE_INTEGER() return _Number.MAX_SAFE_INTEGER;

	/**
		The largest positive representable number.
	**/
	public static var MAX_VALUE(get, never):Float;
	inline static function get_MAX_VALUE() return _Number.MAX_VALUE;

	/**
		The minimum safe integer in JavaScript `(-(2^53 - 1))`.
	**/
	public static var MIN_SAFE_INTEGER(get, never):Int;
	inline static function get_MIN_SAFE_INTEGER() return _Number.MIN_SAFE_INTEGER;

	/**
		The smallest positive representable number - that is, the positive number closest to zero (without actually being zero).
	**/
	public static var MIN_VALUE(get, never):Float;
	inline static function get_MIN_VALUE() return _Number.MIN_VALUE;

	/**
		Special "not a number" value.
	**/
	public static var NaN(get, never):Float;
	inline static function get_NaN() return _Number.NaN;

	/**
		Special value representing negative infinity; returned on overflow.
	**/
	public static var NEGATIVE_INFINITY(get, never):Float;
	inline static function get_NEGATIVE_INFINITY() return _Number.NEGATIVE_INFINITY;

	/**
		Special value representing infinity; returned on overflow.
	**/
	public static var POSITIVE_INFINITY(get, never):Float;
	inline static function get_POSITIVE_INFINITY() return _Number.POSITIVE_INFINITY;

	/**
		Determine whether the passed value is NaN.
	**/
	public static inline function isNaN(value:Any):Bool return _Number.isNaN(value);

	/**
		Determine whether the passed value is a finite number.
	**/
	public static inline function isFinite(value:Any):Bool return _Number.isFinite(value);

	/**
		Determine whether the passed value is an integer.
	**/
	public static inline function isInteger(value:Any):Bool return _Number.isInteger(value);

	/**
		Determine whether the passed value is a safe integer (number between `-(2^53 - 1)` and `(2^53 - 1)`.
	**/
	public static inline function isSafeInteger(testValue:Any):Bool return _Number.isSafeInteger(testValue);

	/**
		The value is the same as `parseFloat()` of the global object.
	**/
	public static inline function parseFloat(string:String):Float return _Number.parseFloat(string);

	/**
		The value is the same as `parseInt()` of the global object. 
	**/
	public static inline function parseInt(string:String, ?radix:Int):Int return _Number.parseInt(string, radix);
}

@:native("Number")
private extern class _Number {
	static inline function fromFloat(value:Float):Number {
		return cast value;
	}

	inline function toFloat():Float {
		return cast this;
	}

	inline function toInt():Int {
		return cast this;
	}

	inline function prefixIncrement():Number {
		return Syntax.code("++{0}", this);
	}

	inline function postfixIncrement():Number {
		return Syntax.code("{0}++", this);
	}

	inline function prefixDecrement():Number {
		return Syntax.code("--{0}", this);
	}

	inline function postfixDecrement():Number {
		return Syntax.code("{0}--", this);
	}

	static final EPSILON:Float;
	static final MAX_SAFE_INTEGER:Int;
	static final MAX_VALUE:Float;
	static final MIN_SAFE_INTEGER:Int;
	static final MIN_VALUE:Float;
	static final NaN:Float;
	static final NEGATIVE_INFINITY:Float;
	static final POSITIVE_INFINITY:Float;

	@:pure static function isNaN(value:Any):Bool;
	@:pure static function isFinite(value:Any):Bool;
	@:pure static function isInteger(value:Any):Bool;
	@:pure static function isSafeInteger(testValue:Any):Bool;
	@:pure static function parseFloat(string:String):Float;
	@:pure static function parseInt(string:String, ?radix:Int):Int;

	@:selfCall
	@:pure function new(value:Any):Void;

	/**
		Returns a string representing the number in exponential notation.
	**/
	@:pure function toExponential(?fractionDigits:Int):Number;

	/**
		Returns a string representing the number in fixed-point notation.
	**/
	@:pure function toFixed(?digits:Int):Number;

	/**
		Returns a string with a language sensitive representation of this number.
		Overrides the Object.prototype.toLocaleString() method.
	**/
	@:overload(function(locales:Array<String>, ?options:NumberFormatOptions):String {})
	@:pure function toLocaleString(?locales:String, ?options:NumberFormatOptions):String;

	/**
		Returns a string representing the number to a specified precision in fixed-point or exponential notation.
	**/
	@:pure function toPrecision(?precision:Int):Number;

	/**
		Returns a string representing the specified object in the specified radix (base).
		Overrides the Object.prototype.toString() method.
	**/
	@:pure function toString():String;
}
