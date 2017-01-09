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
@:forward abstract Short(ShortClass) from ShortClass to ShortClass
{
	@:to @:extern inline public function toShort():java.types.Int16
		return this.shortValue();
	@:from @:extern inline public static function fromShort(b:java.types.Int16):Short
		return ShortClass.valueOf(b);
}

@:native("java.lang.Short") extern class ShortClass extends Number implements Comparable<Short>
{
	@:overload function new(param1 : java.types.Int16) : Void;
	@:overload @:throws("java.lang.NumberFormatException") function new(param1 : String) : Void;
	@:overload function compareTo(param1 : Short) : Int;
	@:overload function compareTo(param1 : Dynamic) : Int;
	@:overload function equals(param1 : Dynamic) : Bool;
	@:overload function hashCode() : Int;
	@:overload function toString() : String;
	@:final static var MAX_VALUE(default,null) : java.types.Int16;
	@:final static var MIN_VALUE(default,null) : java.types.Int16;
	@:final static var SIZE(default,null) : Int;
	@:final static var TYPE : Class<Short>;
	@:overload static function compare(param1 : java.types.Int16, param2 : java.types.Int16) : Int;
	@:overload @:throws("java.lang.NumberFormatException") static function decode(param1 : String) : Short;
	@:overload @:throws("java.lang.NumberFormatException") static function parseShort(param1 : String, param2 : Int) : java.types.Int16;
	@:overload @:throws("java.lang.NumberFormatException") static function parseShort(param1 : String) : java.types.Int16;
	@:overload static function reverseBytes(param1 : java.types.Int16) : java.types.Int16;
	@:native("toString") @:overload static function _toString(param1 : java.types.Int16) : String;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String, param2 : Int) : Short;
	@:overload static function valueOf(param1 : java.types.Int16) : Short;
	@:overload @:throws("java.lang.NumberFormatException") static function valueOf(param1 : String) : Short;
}

@:realPath("java.lang.Short_ShortCache") @:javaNative @:native("java.lang.Short$ShortCache") @:javaCanonical("java.lang","Short.ShortCache") extern class Short_ShortCache {
}
