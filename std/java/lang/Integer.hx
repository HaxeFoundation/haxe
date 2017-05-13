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
@:forward abstract Integer(IntegerClass) from IntegerClass to IntegerClass
{
	@:to @:extern inline public function toInt():Int
		return this.intValue();
	@:from @:extern inline public static function fromInt(b:Int):Integer
		return IntegerClass.valueOf(b);
}

@:native("java.lang.Integer") extern class IntegerClass extends Number implements Comparable<Integer>
{
	@:overload function new(param1 : Int) : Void;
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function compareTo(param1 : Integer) : Int;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function toString() : String;
	@:final static var MAX_VALUE(default,null) : Int;
	@:final static var MIN_VALUE(default,null) : Int;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<Integer>;
	@:overload static function bitCount(param1 : Int) : Int;
	@:overload static function compare(param1 : Int, param2 : Int) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function decode(param1 : String) : Integer;
	@:overload static function getInteger(param1 : String) : Integer;
	@:overload static function getInteger(param1 : String, param2 : Integer) : Integer;
	@:overload static function getInteger(param1 : String, param2 : Int) : Integer;
	@:overload static function highestOneBit(param1 : Int) : Int;
	@:overload static function lowestOneBit(param1 : Int) : Int;
	@:overload static function numberOfLeadingZeros(param1 : Int) : Int;
	@:overload static function numberOfTrailingZeros(param1 : Int) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function parseInt(param1 : String, param2 : Int) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function parseInt(param1 : String) : Int;
	@:overload static function reverse(param1 : Int) : Int;
	@:overload static function reverseBytes(param1 : Int) : Int;
	@:overload static function rotateLeft(param1 : Int, param2 : Int) : Int;
	@:overload static function rotateRight(param1 : Int, param2 : Int) : Int;
	@:overload static function signum(param1 : Int) : Int;
	@:overload static function toBinaryString(param1 : Int) : String;
	@:overload static function toHexString(param1 : Int) : String;
	@:overload static function toOctalString(param1 : Int) : String;
	@:native("toString") @:overload static function _toString(param1 : Int, param2 : Int) : String;
	@:native("toString") @:overload static function _toString(param1 : Int) : String;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String, param2 : Int) : Integer;
	@:overload static function valueOf(param1 : Int) : Integer;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Integer;
}

@:realPath("java.lang.Integer_IntegerCache") @:javaNative @:native("java.lang.Integer$IntegerCache") @:javaCanonical("java.lang","Integer.IntegerCache") extern class Integer_IntegerCache {
}
