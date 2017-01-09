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
@:forward abstract Byte(ByteClass) from ByteClass to ByteClass
{
	@:to @:extern inline public function toByte():java.types.Int8
		return this.byteValue();
	@:from @:extern inline public static function fromByte(b:java.types.Int8):Byte
		return ByteClass.valueOf(b);
}

@:native("java.lang.Byte") extern class ByteClass extends Number implements Comparable<Byte>
{
	@:overload function new(param1 : java.types.Int8) : Void;
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function compareTo(param1 : Byte) : Int;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function toString() : String;
	@:final static var MAX_VALUE(default,null) : java.types.Int8;
	@:final static var MIN_VALUE(default,null) : java.types.Int8;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<Byte>;
	@:overload static function compare(param1 : java.types.Int8, param2 : java.types.Int8) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function decode(param1 : String) : Byte;
	@:overload @:throws("java.lang.NumberFormatException") static function parseByte(param1 : String, param2 : Int) : java.types.Int8;
	@:overload @:throws("java.lang.NumberFormatException") static function parseByte(param1 : String) : java.types.Int8;
	@:native("toString") @:overload static function _toString(param1 : java.types.Int8) : String;
	@:overload static function valueOf(param1 : java.types.Int8) : Byte;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Byte;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String, param2 : Int) : Byte;
}

@:realPath("java.lang.Byte_ByteCache") @:javaNative @:native("java.lang.Byte$ByteCache") @:javaCanonical("java.lang","Byte.ByteCache") extern class Byte_ByteCache {
}
