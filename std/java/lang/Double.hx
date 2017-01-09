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
 package java.lang;

@:native("") // make sure the generator won't see this
@:forwardStatics
@:forward abstract Double(DoubleClass) from DoubleClass to DoubleClass
{
	@:to @:extern inline public function toFloat():Float
		return this.doubleValue();
	@:from @:extern inline public static function fromFloat(b:Float):Double
		return DoubleClass.valueOf(b);
}

@:native("java.lang.Double") extern class DoubleClass extends Number implements Comparable<Double>
{
	@:overload function new(param1 : Float) : Void;
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function compareTo(param1 : Double) : Int;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function isInfinite() : Bool;
	@:overload function isNaN() : Bool;
	@:overload function toString() : String;
	@:final static var MAX_EXPONENT(default,null) : Int;
	@:final static var MAX_VALUE(default,null) : Float;
	@:final static var MIN_EXPONENT(default,null) : Int;
	@:final static var MIN_NORMAL(default,null) : Float;
	@:final static var MIN_VALUE(default,null) : Float;
	@:final static var NEGATIVE_INFINITY(default,null) : Float;
	@:final static var NaN(default,null) : Float;
	@:final static var POSITIVE_INFINITY(default,null) : Float;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<Double>;
	@:overload static function compare(param1 : Float, param2 : Float) : Int;
	@:overload static function doubleToLongBits(param1 : Float) : haxe.Int64;
	@:overload static function doubleToRawLongBits(param1 : Float) : haxe.Int64;
	@:native("isInfinite") @:overload static function _isInfinite(param1 : Float) : Bool;
	@:native("isNaN") @:overload static function _isNaN(param1 : Float) : Bool;
	@:overload static function longBitsToDouble(param1 : haxe.Int64) : Float;
	@:overload @:throws("java.lang.NumberFormatException") static function parseDouble(param1 : String) : Float;
	@:overload static function toHexString(param1 : Float) : String;
	@:native("toString") @:overload static function _toString(param1 : Float) : String;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Double;
	@:overload static function valueOf(param1 : Float) : Double;
}
