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
@:forward abstract Float(FloatClass) from FloatClass to FloatClass
{
	@:to @:extern inline public function toFloat():std.StdTypes.Float
		return this.floatValue();
	@:from @:extern inline public static function fromFloat(b:std.StdTypes.Single):Float
		return FloatClass.valueOf(b);
}

@:native("java.lang.Float") extern class FloatClass extends Number implements Comparable<Float>
{
	@:overload function new(param1 : Single) : Void;
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function new(param1 : std.StdTypes.Float) : Void;
	@:overload function compareTo(param1 : Float) : Int;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function isInfinite() : Bool;
	@:overload function isNaN() : Bool;
	@:overload function toString() : String;
	@:final static var MAX_EXPONENT(default,null) : Int;
	@:final static var MAX_VALUE(default,null) : Single;
	@:final static var MIN_EXPONENT(default,null) : Int;
	@:final static var MIN_NORMAL(default,null) : Single;
	@:final static var MIN_VALUE(default,null) : Single;
	@:final static var NEGATIVE_INFINITY(default,null) : Single;
	@:final static var NaN(default,null) : Single;
	@:final static var POSITIVE_INFINITY(default,null) : Single;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<std.StdTypes.Float>;
	@:overload static function compare(param1 : Single, param2 : Single) : Int;
	@:overload static function floatToIntBits(param1 : Single) : Int;
	@:overload static function floatToRawIntBits(param1 : Single) : Int;
	@:overload static function intBitsToFloat(param1 : Int) : Single;
	@:native("isInfinite") @:overload static function _isInfinite(param1 : Single) : Bool;
	@:native("isNaN") @:overload static function _isNaN(param1 : Single) : Bool;
	@:overload @:throws("java.lang.NumberFormatException") static function parseFloat(param1 : String) : Single;
	@:overload static function toHexString(param1 : Single) : String;
	@:native("toString") @:overload static function _toString(param1 : Single) : String;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Float;
	@:overload static function valueOf(param1 : Single) : Float;

}

