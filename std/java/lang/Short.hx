package java.lang;

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
