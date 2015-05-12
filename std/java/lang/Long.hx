package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Long(LongClass) from LongClass to LongClass
{
	@:to @:extern inline public function toLong():haxe.Int64
		return this.longValue();
	@:from @:extern inline public static function fromLong(b:haxe.Int64):Long
		return LongClass.valueOf(b);

	public static var MAX_VALUE(get,never):haxe.Int64;
	@:extern static inline function get_MAX_VALUE():haxe.Int64 return LongClass.MAX_VALUE;
	public static var MIN_VALUE(get,never):haxe.Int64;
	@:extern static inline function get_MIN_VALUE():haxe.Int64 return LongClass.MIN_VALUE;
	public static var SIZE(get,never):Int;
	@:extern static inline function get_SIZE():Int return LongClass.SIZE;
	public static var TYPE(get,set):Class<java.lang.Long>;
	@:extern static inline function get_TYPE():Class<java.lang.Long> return LongClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<java.lang.Long>):Class<java.lang.Long> return LongClass.TYPE = val;
	@:extern @:overload inline public static function bitCount(param1:haxe.Int64):Int return LongClass.bitCount(param1);
	@:extern @:overload inline public static function compare(param1:haxe.Int64, param2:haxe.Int64):Int return LongClass.compare(param1, param2);
	@:extern @:overload inline public static function decode(param1:String):Long return LongClass.decode(param1);
	@:extern @:overload inline public static function getLong(param1:String, param2:Long):Long return LongClass.getLong(param1, param2);
	@:extern @:overload inline public static function highestOneBit(param1:haxe.Int64):haxe.Int64 return LongClass.highestOneBit(param1);
	@:extern @:overload inline public static function lowestOneBit(param1:haxe.Int64):haxe.Int64 return LongClass.lowestOneBit(param1);
	@:extern @:overload inline public static function numberOfLeadingZeros(param1:haxe.Int64):Int return LongClass.numberOfLeadingZeros(param1);
	@:extern @:overload inline public static function numberOfTrailingZeros(param1:haxe.Int64):Int return LongClass.numberOfTrailingZeros(param1);
	@:extern @:overload inline public static function parseLong(param1:String):haxe.Int64 return LongClass.parseLong(param1);
	@:extern @:overload inline public static function reverse(param1:haxe.Int64):haxe.Int64 return LongClass.reverse(param1);
	@:extern @:overload inline public static function reverseBytes(param1:haxe.Int64):haxe.Int64 return LongClass.reverseBytes(param1);
	@:extern @:overload inline public static function rotateLeft(param1:haxe.Int64, param2:Int):haxe.Int64 return LongClass.rotateLeft(param1, param2);
	@:extern @:overload inline public static function rotateRight(param1:haxe.Int64, param2:Int):haxe.Int64 return LongClass.rotateRight(param1, param2);
	@:extern @:overload inline public static function signum(param1:haxe.Int64):Int return LongClass.signum(param1);
	@:extern @:overload inline public static function toBinaryString(param1:haxe.Int64):String return LongClass.toBinaryString(param1);
	@:extern @:overload inline public static function toHexString(param1:haxe.Int64):String return LongClass.toHexString(param1);
	@:extern @:overload inline public static function toOctalString(param1:haxe.Int64):String return LongClass.toOctalString(param1);
	@:extern @:overload inline public static function _toString(param1:haxe.Int64):String return LongClass._toString(param1);
	@:extern @:overload inline public static function valueOf(param1:haxe.Int64):Long return LongClass.valueOf(param1);
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
