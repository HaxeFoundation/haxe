package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Integer(IntegerClass) from IntegerClass to IntegerClass
{
	@:to @:extern inline public function toInt():Int
		return this.intValue();
	@:from @:extern inline public static function fromInt(b:Int):Integer
		return IntegerClass.valueOf(b);

	public static var MAX_VALUE(get,never):Int;
	@:extern static inline function get_MAX_VALUE():Int return IntegerClass.MAX_VALUE;
	public static var MIN_VALUE(get,never):Int;
	@:extern static inline function get_MIN_VALUE():Int return IntegerClass.MIN_VALUE;
	public static var SIZE(get,never):Int;
	@:extern static inline function get_SIZE():Int return IntegerClass.SIZE;
	public static var TYPE(get,set):Class<java.lang.Integer>;
	@:extern static inline function get_TYPE():Class<java.lang.Integer> return IntegerClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<java.lang.Integer>):Class<java.lang.Integer> return IntegerClass.TYPE = val;
	@:extern @:overload inline public static function bitCount(param1:Int):Int return IntegerClass.bitCount(param1);
	@:extern @:overload inline public static function compare(param1:Int, param2:Int):Int return IntegerClass.compare(param1, param2);
	@:extern @:overload inline public static function decode(param1:String):Integer return IntegerClass.decode(param1);
	@:extern @:overload inline public static function getInteger(param1:String):Integer return IntegerClass.getInteger(param1);
	@:extern @:overload inline public static function highestOneBit(param1:Int):Int return IntegerClass.highestOneBit(param1);
	@:extern @:overload inline public static function lowestOneBit(param1:Int):Int return IntegerClass.lowestOneBit(param1);
	@:extern @:overload inline public static function numberOfLeadingZeros(param1:Int):Int return IntegerClass.numberOfLeadingZeros(param1);
	@:extern @:overload inline public static function numberOfTrailingZeros(param1:Int):Int return IntegerClass.numberOfTrailingZeros(param1);
	@:extern @:overload inline public static function parseInt(param1:String, param2:Int):Int return IntegerClass.parseInt(param1, param2);
	@:extern @:overload inline public static function reverse(param1:Int):Int return IntegerClass.reverse(param1);
	@:extern @:overload inline public static function reverseBytes(param1:Int):Int return IntegerClass.reverseBytes(param1);
	@:extern @:overload inline public static function rotateLeft(param1:Int, param2:Int):Int return IntegerClass.rotateLeft(param1, param2);
	@:extern @:overload inline public static function rotateRight(param1:Int, param2:Int):Int return IntegerClass.rotateRight(param1, param2);
	@:extern @:overload inline public static function signum(param1:Int):Int return IntegerClass.signum(param1);
	@:extern @:overload inline public static function toBinaryString(param1:Int):String return IntegerClass.toBinaryString(param1);
	@:extern @:overload inline public static function toHexString(param1:Int):String return IntegerClass.toHexString(param1);
	@:extern @:overload inline public static function toOctalString(param1:Int):String return IntegerClass.toOctalString(param1);
	@:extern @:overload inline public static function _toString(param1:Int, param2:Int):String return IntegerClass._toString(param1, param2);
	@:extern @:overload inline public static function valueOf(param1:String, param2:Int):Integer return IntegerClass.valueOf(param1, param2);
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
