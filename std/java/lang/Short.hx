package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Short(ShortClass) from ShortClass to ShortClass
{
	@:to @:extern inline public function toShort():java.types.Int16
		return this.shortValue();
	@:from @:extern inline public static function fromShort(b:java.types.Int16):Short
		return ShortClass.valueOf(b);

	public static var MAX_VALUE(get,never):java.types.Int16;
	@:extern static inline function get_MAX_VALUE():java.types.Int16 return ShortClass.MAX_VALUE;
	public static var MIN_VALUE(get,never):java.types.Int16;
	@:extern static inline function get_MIN_VALUE():java.types.Int16 return ShortClass.MIN_VALUE;
	public static var SIZE(get,never):Int;
	@:extern static inline function get_SIZE():Int return ShortClass.SIZE;
	public static var TYPE(get,set):Class<java.lang.Short>;
	@:extern static inline function get_TYPE():Class<java.lang.Short> return ShortClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<java.lang.Short>):Class<java.lang.Short> return ShortClass.TYPE = val;
	@:extern @:overload inline public static function compare(param1:java.types.Int16, param2:java.types.Int16):Int return ShortClass.compare(param1, param2);
	@:extern @:overload inline public static function decode(param1:String):Short return ShortClass.decode(param1);
	@:extern @:overload inline public static function parseShort(param1:String, param2:Int):java.types.Int16 return ShortClass.parseShort(param1, param2);
	@:extern @:overload inline public static function reverseBytes(param1:java.types.Int16):java.types.Int16 return ShortClass.reverseBytes(param1);
	@:extern @:overload inline public static function _toString(param1:java.types.Int16):String return ShortClass._toString(param1);
	@:extern @:overload inline public static function valueOf(param1:String, param2:Int):Short return ShortClass.valueOf(param1, param2);
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
