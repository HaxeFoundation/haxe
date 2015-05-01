package java.lang;

@:native("") // make sure the generator won't see this
@:forward abstract Boolean(BooleanClass) from BooleanClass to BooleanClass
{
	@:to @:extern inline public function toBool():Bool
		return this.booleanValue();
	@:from @:extern inline public static function fromBool(b:Bool):Boolean
		return BooleanClass.valueOf(b);

	public static var FALSE(get,set):Boolean;
	@:extern static inline function get_FALSE():Boolean return BooleanClass.FALSE;
	@:extern static inline function set_FALSE(val:Boolean):Boolean return BooleanClass.FALSE = val;
	public static var TRUE(get,set):Boolean;
	@:extern static inline function get_TRUE():Boolean return BooleanClass.TRUE;
	@:extern static inline function set_TRUE(val:Boolean):Boolean return BooleanClass.TRUE = val;
	public static var TYPE(get,set):Class<java.lang.Boolean>;
	@:extern static inline function get_TYPE():Class<java.lang.Boolean> return BooleanClass.TYPE;
	@:extern static inline function set_TYPE(val:Class<java.lang.Boolean>):Class<java.lang.Boolean> return BooleanClass.TYPE = val;
	@:extern @:overload inline public static function compare(param1:Bool, param2:Bool):Int return BooleanClass.compare(param1, param2);
	@:extern @:overload inline public static function getBoolean(param1:String):Bool return BooleanClass.getBoolean(param1);
	@:extern @:overload inline public static function parseBoolean(param1:String):Bool return BooleanClass.parseBoolean(param1);
	@:extern @:overload inline public static function _toString(param1:Bool):String return BooleanClass._toString(param1);
	@:extern @:overload inline public static function valueOf(param1:Bool):Boolean return BooleanClass.valueOf(param1);
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
