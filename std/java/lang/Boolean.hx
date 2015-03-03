package java.lang;

@:forward abstract Boolean(BooleanClass) from BooleanClass to BooleanClass
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
