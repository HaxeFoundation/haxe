enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:coreApi
class Type {

	@:hlNative("std","type_get_class")
	public static function getClass<T>( o : T ) : Class<T> {
		return null;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> {
		throw "TODO";
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		throw "TODO";
		return null;
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		var c : hl.types.Class = cast c;
		return c.__name__;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		throw "TODO";
		return null;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		throw "TODO";
		return null;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		throw "TODO";
		return null;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		throw "TODO";
		return null;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		throw "TODO";
		return null;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		throw "TODO";
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		throw "TODO";
		return null;
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> @:privateAccess {
		var c : hl.types.Class = cast c;
		var fields = c.type.getInstanceFields();
		return [for( f in fields ) String.__alloc__(f,f.ucs2Length(0))];
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		throw "TODO";
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		throw "TODO";
		return null;
	}

	public static function typeof( v : Dynamic ) : ValueType {
		var t = hl.types.Type.getDynamic(v);
		switch( t.kind ) {
		case HVoid:
			return TNull;
		case HI8, HI16, HI32:
			return TInt;
		case HF32, HF64:
			return TFloat;
		case HBool:
			return TBool;
		case HDynObj:
			return TObject;
		default:
			return TUnknown;
		}
	}

	@:hlNative("std","type_enum_eq")
	public static function enumEq<T:EnumValue>( a : T, b : T ) : Bool {
		return false;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		throw "TODO";
		return null;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		throw "TODO";
		return null;
	}

	public static function enumIndex( e : EnumValue ) : Int {
		throw "TODO";
		return 0;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		throw "TODO";
		return null;
	}

}