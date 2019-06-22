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

import js.lib.intl.NumberFormat.NumberFormatOptions;

@:native("Number")
extern class Number {
	/**
		Cast Haxe's Int to js.lib.Number.
	**/
	@:pure public static inline function fromInt(value:Int):Number {
		return cast value;
	}

	/**
		Cast js.lib.Number to Haxe's Int.
	**/
	@:pure public inline function toInt():Int {
		return cast this;
	}

	/**
		Cast Haxe's Float to js.lib.Number.
	**/
	@:pure public static inline function fromFloat(value:Float):Number {
		return cast value;
	}

	/**
		Cast js.lib.Number to Haxe's Float.
	**/
	@:pure public inline function toFloat():Float {
		return cast this;
	}

	/**
		The smallest interval between two representable numbers.
	**/
	static final EPSILON:Float;

	/**
		The maximum safe integer in JavaScript `(2^53 - 1)`.
	**/
	static final MAX_SAFE_INTEGER:Int;

	/**
		The largest positive representable number.
	**/
	static final MAX_VALUE:Float;

	/**
		The minimum safe integer in JavaScript `(-(2^53 - 1))`.
	**/
	static final MIN_SAFE_INTEGER:Int;

	/**
		The smallest positive representable number - that is, the positive number closest to zero (without actually being zero).
	**/
	static final MIN_VALUE:Float;

	/**
		Special "not a number" value.
	**/
	static final NaN:Float;

	/**
		Special value representing negative infinity; returned on overflow.
	**/
	static final NEGATIVE_INFINITY:Float;

	/**
		Special value representing infinity; returned on overflow.
	**/
	static final POSITIVE_INFINITY:Float;

	/**
		Determine whether the passed value is NaN.
	**/
	@:pure static function isNaN(value:Any):Bool;

	/**
		Determine whether the passed value is a finite number.
	**/
	@:pure static function isFinite(value:Any):Bool;

	/**
		Determine whether the passed value is an integer.
	**/
	@:pure static function isInteger(value:Any):Bool;

	/**
		Determine whether the passed value is a safe integer (number between `-(2^53 - 1)` and `(2^53 - 1)`.
	**/
	@:pure static function isSafeInteger(testValue:Any):Bool;

	/**
		The value is the same as `parseFloat()` of the global object.
	**/
	@:pure static function parseFloat(string:String):Float;

	/**
		The value is the same as `parseInt()` of the global object. 
	**/
	static function parseInt(string:String, ?radix:Int):Int;

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
