package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Float(FloatClass) from FloatClass to FloatClass
{
	@:to @:extern inline public function toFloat():std.StdTypes.Float
		return this.floatValue();
	@:from @:extern inline public static function fromFloat(b:std.StdTypes.Single):Float
		return FloatClass.valueOf(b);

	public static var MAX_EXPONENT(get,never):Int;
	@:extern static inline function get_MAX_EXPONENT():Int return FloatClass.MAX_EXPONENT;
	public static var MAX_VALUE(get,never):Single;
	@:extern static inline function get_MAX_VALUE():Single return FloatClass.MAX_VALUE;
	public static var MIN_EXPONENT(get,never):Int;
	@:extern static inline function get_MIN_EXPONENT():Int return FloatClass.MIN_EXPONENT;
	public static var MIN_NORMAL(get,never):Single;
	@:extern static inline function get_MIN_NORMAL():Single return FloatClass.MIN_NORMAL;
	public static var MIN_VALUE(get,never):Single;
	@:extern static inline function get_MIN_VALUE():Single return FloatClass.MIN_VALUE;
	public static var NEGATIVE_INFINITY(get,never):Single;
	@:extern static inline function get_NEGATIVE_INFINITY():Single return FloatClass.NEGATIVE_INFINITY;
	public static var NaN(get,never):Single;
	@:extern static inline function get_NaN():Single return FloatClass.NaN;
	public static var POSITIVE_INFINITY(get,never):Single;
	@:extern static inline function get_POSITIVE_INFINITY():Single return FloatClass.POSITIVE_INFINITY;
	public static var SIZE(get,never):Int;
	@:extern static inline function get_SIZE():Int return FloatClass.SIZE;
	public static var TYPE(get,set):Class<std.StdTypes.Float>;
	@:extern static inline function get_TYPE():Class<std.StdTypes.Float> return FloatClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<std.StdTypes.Float>):Class<std.StdTypes.Float> return FloatClass.TYPE = val;
	@:extern @:overload inline public static function compare(param1:Single, param2:Single):Int return FloatClass.compare(param1, param2);
	@:extern @:overload inline public static function floatToIntBits(param1:Single):Int return FloatClass.floatToIntBits(param1);
	@:extern @:overload inline public static function floatToRawIntBits(param1:Single):Int return FloatClass.floatToRawIntBits(param1);
	@:extern @:overload inline public static function intBitsToFloat(param1:Int):Single return FloatClass.intBitsToFloat(param1);
	@:extern @:overload inline public static function _isInfinite(param1:Single):Bool return FloatClass._isInfinite(param1);
	@:extern @:overload inline public static function _isNaN(param1:Single):Bool return FloatClass._isNaN(param1);
	@:extern @:overload inline public static function parseFloat(param1:String):Single return FloatClass.parseFloat(param1);
	@:extern @:overload inline public static function toHexString(param1:Single):String return FloatClass.toHexString(param1);
	@:extern @:overload inline public static function _toString(param1:Single):String return FloatClass._toString(param1);
	@:extern @:overload inline public static function valueOf(param1:String):Float return FloatClass.valueOf(param1);
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

