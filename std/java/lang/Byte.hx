package java.lang;

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
