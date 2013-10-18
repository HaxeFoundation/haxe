class Type {
	public static function allEnums<T>(e:Enum<T>):Array<T> {
		return untyped e.__all();
	}
	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		return untyped e.__constructs();
	}
	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		return cast untyped e.__create(constr, params);
	}
	public static function createEnumIndex<T>(e:Enum<T>, constr:Int, ?params:Array<Dynamic>):T {
		return cast untyped e.__createInd(constr, params);
	}
	public static function getClassName(e:Class<Dynamic>):String {
		return untyped e.__name();
	}
	public static function getEnumName(e:Enum<Dynamic>):String {
		return untyped e.__name();
	}
	public static function enumConstructor(e:EnumValue):String {
		return untyped e.__constructor();
	}
	public static function enumIndex(e:EnumValue):Int {
		return untyped e.__index();
	}
	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		return untyped e.__parameters();
	}
	public static inline function enumEq<T>(a:T, b:T):Bool {
		return a == b;
	}
}