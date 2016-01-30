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

	static var allTypes(get,never) : hl.types.NativeBytesMap;
	static inline function get_allTypes() : hl.types.NativeBytesMap return untyped $allTypes();
	
	@:keep static function init() : Void {
		untyped $allTypes(new hl.types.NativeBytesMap());
	}

	@:keep static function register( b : hl.types.Bytes, t : hl.types.BaseType ) : Void {
		allTypes.set(b, t);
	}

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
		var c : hl.types.BaseType.Class = cast c;
		return c.__name__;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		var e : hl.types.BaseType.Enum = cast e;
		return e.__ename__;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		var t : hl.types.BaseType = allTypes.get(@:privateAccess name.bytes);
		if( t == null || !Std.is(t, hl.types.BaseType.Class) )
			return null;
		return cast t;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		var t : hl.types.BaseType = allTypes.get(@:privateAccess name.bytes);
		if( t == null || !Std.is(t, hl.types.BaseType.Enum) )
			return null;
		return cast t;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		var c : hl.types.BaseType.Class = cast cl;
		var o = c.__type__.allocObject();
		if( c.__constructor__ != null ) Reflect.callMethod(o, c.__constructor__, args);
		return o;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		var c : hl.types.BaseType.Class = cast cl;
		return c.__type__.allocObject();
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var en : hl.types.BaseType.Enum = cast e;
		var idx : Null<Int> = en.__emap__.get(@:privateAccess constr.bytes);
		if( idx == null ) throw "Unknown enum constructor " + en.__ename__ +"." + constr;
		return createEnumIndex(e,idx,params);
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var e : hl.types.BaseType.Enum = cast e;
		if( index < 0 || index >= e.__constructs__.length ) throw "Invalid enum index " + e.__ename__ +"." + index;
		if( params == null ) {
			var v = index >= e.__evalues__.length ? null : e.__evalues__[index];
			if( v == null ) throw "Constructor " + e.__ename__ +"." + e.__constructs__[index] + " takes parameters";
			return v;
		}
		var a : hl.types.ArrayDyn = cast params;
		var aobj = Std.instance(@:privateAccess a.array, hl.types.ArrayObj);
		var narr;
		if( aobj == null ) {
			narr = new hl.types.NativeArray<Dynamic>(a.length);
			for( i in 0...a.length )
				narr[i] = @:privateAccess a.array.getDyn(i);
		} else {
			narr = @:privateAccess aobj.array;
		}
		return @:privateAccess e.__type__.allocEnum(index, narr);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> @:privateAccess {
		var c : hl.types.BaseType.Class = cast c;
		var fields = c.__type__.getInstanceFields();
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
		case HObj:
			return TClass(Type.getClass(v));
		case HFun:
			return TFunction;
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