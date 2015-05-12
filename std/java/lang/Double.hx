package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Double(DoubleClass) from DoubleClass to DoubleClass
{
	@:to @:extern inline public function toFloat():Float
		return this.doubleValue();
	@:from @:extern inline public static function fromFloat(b:Float):Double
		return DoubleClass.valueOf(b);

	public static var MAX_EXPONENT(get,never):Int;
	@:extern static inline function get_MAX_EXPONENT():Int return DoubleClass.MAX_EXPONENT;
	public static var MAX_VALUE(get,never):Float;
	@:extern static inline function get_MAX_VALUE():Float return DoubleClass.MAX_VALUE;
	public static var MIN_EXPONENT(get,never):Int;
	@:extern static inline function get_MIN_EXPONENT():Int return DoubleClass.MIN_EXPONENT;
	public static var MIN_NORMAL(get,never):Float;
	@:extern static inline function get_MIN_NORMAL():Float return DoubleClass.MIN_NORMAL;
	public static var MIN_VALUE(get,never):Float;
	@:extern static inline function get_MIN_VALUE():Float return DoubleClass.MIN_VALUE;
	public static var NEGATIVE_INFINITY(get,never):Float;
	@:extern static inline function get_NEGATIVE_INFINITY():Float return DoubleClass.NEGATIVE_INFINITY;
	public static var NaN(get,never):Float;
	@:extern static inline function get_NaN():Float return DoubleClass.NaN;
	public static var POSITIVE_INFINITY(get,never):Float;
	@:extern static inline function get_POSITIVE_INFINITY():Float return DoubleClass.POSITIVE_INFINITY;
	public static var SIZE(get,never):Int;
	@:extern static inline function get_SIZE():Int return DoubleClass.SIZE;
	public static var TYPE(get,set):Class<java.lang.Double>;
	@:extern static inline function get_TYPE():Class<java.lang.Double> return DoubleClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<java.lang.Double>):Class<java.lang.Double> return DoubleClass.TYPE = val;
	@:extern @:overload inline public static function compare(param1:Float, param2:Float):Int return DoubleClass.compare(param1, param2);
	@:extern @:overload inline public static function doubleToLongBits(param1:Float):haxe.Int64 return DoubleClass.doubleToLongBits(param1);
	@:extern @:overload inline public static function doubleToRawLongBits(param1:Float):haxe.Int64 return DoubleClass.doubleToRawLongBits(param1);
	@:extern @:overload inline public static function _isInfinite(param1:Float):Bool return DoubleClass._isInfinite(param1);
	@:extern @:overload inline public static function _isNaN(param1:Float):Bool return DoubleClass._isNaN(param1);
	@:extern @:overload inline public static function longBitsToDouble(param1:haxe.Int64):Float return DoubleClass.longBitsToDouble(param1);
	@:extern @:overload inline public static function parseDouble(param1:String):Float return DoubleClass.parseDouble(param1);
	@:extern @:overload inline public static function toHexString(param1:Float):String return DoubleClass.toHexString(param1);
	@:extern @:overload inline public static function _toString(param1:Float):String return DoubleClass._toString(param1);
	@:extern @:overload inline public static function valueOf(param1:String):Double return DoubleClass.valueOf(param1);
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
