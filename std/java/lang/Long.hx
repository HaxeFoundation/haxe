package java.lang;

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
