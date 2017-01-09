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
@:forward
@:forwardStatics abstract Boolean(BooleanClass) from BooleanClass to BooleanClass
{
	@:to @:extern inline public function toBool():Bool
		return this.booleanValue();
	@:from @:extern inline public static function fromBool(b:Bool):Boolean
		return BooleanClass.valueOf(b);
}

@:native("java.lang.Boolean") extern class BooleanClass extends Number implements Comparable<Boolean>
{
	@:overload public function new(bool:Bool):Void;
	@:overload public function new(string:String):Void;
	@:overload public function booleanValue() : Bool;
	@:overload public function compareTo(param1 : Boolean) : Int;
	@:overload public function compareTo(param1 : Dynamic) : Int;
	@:overload public function equals(param1 : Dynamic) : Bool;
	@:overload public function hashCode() : Int;
	@:overload public function toString() : String;
	@:final public static var FALSE : Boolean;
	@:final public static var TRUE : Boolean;
	@:final public static var TYPE : Class<Boolean>;
	@:overload public static function compare(param1 : Bool, param2 : Bool) : Int;
	@:overload public static function getBoolean(param1 : String) : Bool;
	@:overload public static function parseBoolean(param1 : String) : Bool;
	@:native("toString") @:overload public static function _toString(param1 : Bool) : String;
	@:overload public static function valueOf(param1 : Bool) : Boolean;
	@:overload public static function valueOf(param1 : String) : Boolean;
}
