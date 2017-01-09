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
@:forward abstract Long(LongClass) from LongClass to LongClass
{
	@:to @:extern inline public function toLong():haxe.Int64
		return this.longValue();
	@:from @:extern inline public static function fromLong(b:haxe.Int64):Long
		return LongClass.valueOf(b);
}

@:native("java.lang.Long") extern class LongClass extends Number implements Comparable<Long>
{
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function new(param1 : haxe.Int64) : Void;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function compareTo(param1 : Long) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function toString() : String;
	@:final static var MAX_VALUE(default,null) : haxe.Int64;
	@:final static var MIN_VALUE(default,null) : haxe.Int64;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<Long>;
	@:overload static function bitCount(param1 : haxe.Int64) : Int;
	@:overload static function compare(param1 : haxe.Int64, param2 : haxe.Int64) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function decode(param1 : String) : Long;
	@:overload static function getLong(param1 : String, param2 : Long) : Long;
	@:overload static function getLong(param1 : String) : Long;
	@:overload static function getLong(param1 : String, param2 : haxe.Int64) : Long;
	@:overload static function highestOneBit(param1 : haxe.Int64) : haxe.Int64;
	@:overload static function lowestOneBit(param1 : haxe.Int64) : haxe.Int64;
	@:overload static function numberOfLeadingZeros(param1 : haxe.Int64) : Int;
	@:overload static function numberOfTrailingZeros(param1 : haxe.Int64) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function parseLong(param1 : String) : haxe.Int64;
	@:overload @:throws("java.lang.NumberFormatException") static function parseLong(param1 : String, param2 : Int) : haxe.Int64;
	@:overload static function reverse(param1 : haxe.Int64) : haxe.Int64;
	@:overload static function reverseBytes(param1 : haxe.Int64) : haxe.Int64;
	@:overload static function rotateLeft(param1 : haxe.Int64, param2 : Int) : haxe.Int64;
	@:overload static function rotateRight(param1 : haxe.Int64, param2 : Int) : haxe.Int64;
	@:overload static function signum(param1 : haxe.Int64) : Int;
	@:overload static function toBinaryString(param1 : haxe.Int64) : String;
	@:overload static function toHexString(param1 : haxe.Int64) : String;
	@:overload static function toOctalString(param1 : haxe.Int64) : String;
	@:native("toString") @:overload static function _toString(param1 : haxe.Int64) : String;
	@:native("toString") @:overload static function _toString(param1 : haxe.Int64, param2 : Int) : String;
	@:overload static function valueOf(param1 : haxe.Int64) : Long;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String, param2 : Int) : Long;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Long;
}

@:realPath("java.lang.Long_LongCache") @:javaNative @:native("java.lang.Long$LongCache") @:javaCanonical("java.lang","Long.LongCache") extern class Long_LongCache {
}
