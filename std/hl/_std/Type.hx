/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

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

	static var allTypes(get,never) : hl.types.BytesMap;
	static inline function get_allTypes() : hl.types.BytesMap return untyped $allTypes();

	@:keep static function init() : Void {
		untyped $allTypes(new hl.types.BytesMap());
	}

	@:keep static function initClass( ct : hl.Type, t : hl.Type, name : hl.Bytes ) : hl.BaseType.Class @:privateAccess {
		var c : hl.BaseType.Class = ct.allocObject();
		t.setGlobal(c);
		c.__type__ = t;
		c.__name__ = String.fromUCS2(name);
		register(name, c);
		return c;
	}

	@:keep static function initEnum( et : hl.Type, t : hl.Type ) : hl.BaseType.Enum @:privateAccess {
		var e : hl.BaseType.Enum = et.allocObject();
		e.__type__ = t;
		e.__evalues__ = t.getEnumValues();
		e.__ename__ = t.getName();
		e.__emap__ = new hl.types.BytesMap();
		e.__constructs__ = new Array();
		var cl = t.getEnumFields();
		for( i in 0...cl.length ) {
			var name = cl[i];
			e.__emap__.set(name, i);
			e.__constructs__.push(String.fromUCS2(name));
		}
		register(e.__ename__.bytes,e);
		t.setGlobal(e);
		return e;
	}

	@:keep static function register( b : hl.Bytes, t : hl.BaseType ) : Void {
		allTypes.set(b, t);
	}

	public static function getClass<T>( o : T ) : Class<T> {
		var t = hl.Type.getDynamic(o);
		if( t.kind == HObj )
			return t.getGlobal();
		return null;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> {
		var t = hl.Type.getDynamic(o);
		if( t.kind == HEnum )
			return t.getGlobal();
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> @:privateAccess {
		var c : hl.BaseType.Class = cast c;
		var t = c.__type__.getSuper();
		return t == hl.Type.get((null : Void)) ? null : t.getGlobal();
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		var c : hl.BaseType.Class = cast c;
		return c.__name__;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		var e : hl.BaseType.Enum = cast e;
		return e.__ename__;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		var t : hl.BaseType = allTypes.get(@:privateAccess name.bytes);
		if( t == null || !Std.is(t, hl.BaseType.Class) )
			return null;
		return cast t;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		var t : hl.BaseType = allTypes.get(@:privateAccess name.bytes);
		if( t == null || !Std.is(t, hl.BaseType.Enum) )
			return null;
		return cast t;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		var c : hl.BaseType.Class = cast cl;
		var t = c.__type__;
		if( t == hl.Type.get((null : hl.types.ArrayBase.ArrayAccess)) )
			return cast new Array<Dynamic>();
		var o = t.allocObject();
		if( c.__constructor__ != null ) Reflect.callMethod(o, c.__constructor__, args);
		return o;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		var c : hl.BaseType.Class = cast cl;
		return c.__type__.allocObject();
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		var en : hl.BaseType.Enum = cast e;
		var idx : Null<Int> = en.__emap__.get(@:privateAccess constr.bytes);
		if( idx == null ) throw "Unknown enum constructor " + en.__ename__ +"." + constr;
		return createEnumIndex(e,idx,params);
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var e : hl.BaseType.Enum = cast e;
		if( index < 0 || index >= e.__constructs__.length ) throw "Invalid enum index " + e.__ename__ +"." + index;
		if( params == null || params.length == 0 ) {
			var v = index >= e.__evalues__.length ? null : e.__evalues__[index];
			if( v == null ) throw "Constructor " + e.__ename__ +"." + e.__constructs__[index] + " takes parameters";
			return v;
		}
		var a : hl.types.ArrayDyn = cast params;
		var narr;
		if( @:privateAccess !a.array.isArrayObj() ) {
			narr = new hl.NativeArray<Dynamic>(a.length);
			for( i in 0...a.length )
				narr[i] = @:privateAccess a.array.getDyn(i);
		} else {
			var aobj : hl.types.ArrayObj<Dynamic> = cast @:privateAccess a.array;
			narr = @:privateAccess aobj.array;
		}
		var v = @:privateAccess e.__type__.allocEnum(index, narr, a.length);
		if( v == null ) throw "Constructor " + e.__ename__ +"." + e.__constructs__[index] + " does not takes " + narr.length + " parameters";
		return v;
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> @:privateAccess {
		var c : hl.BaseType.Class = cast c;
		var fields = c.__type__.getInstanceFields();
		return [for( f in fields ) String.fromUCS2(f)];
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		var c : hl.BaseType.Class = cast c;
		var fields = @:privateAccess Reflect.getObjectFields(c);
		var fields = [for( f in fields ) @:privateAccess String.fromUCS2(f)];
		fields.remove("__constructor__");
		fields.remove("__meta__");
		fields.remove("__name__");
		fields.remove("__type__");
		fields.remove("__implementedBy__");
		return fields;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		var e : hl.BaseType.Enum = cast e;
		return e.__constructs__.copy();
	}

	public static function typeof( v : Dynamic ) : ValueType {
		var t = hl.Type.getDynamic(v);
		switch( t.kind ) {
		case HVoid:
			return TNull;
		case HUI8, HUI16, HI32:
			return TInt;
		case HF32, HF64:
			return TFloat;
		case HBool:
			return TBool;
		case HDynObj:
			return TObject;
		case HObj:
			var c : Dynamic = Type.getClass(v);
			if( c == Class || c == null )
				return TObject;
			return TClass(c);
		case HEnum:
			return TEnum(Type.getEnum(v));
		case HFun:
			return TFunction;
		case HVirtual:
			var v = hl.Api.getVirtualValue(v);
			if( v == null )
				return TObject;
			return typeof(v);
		default:
			return TUnknown;
		}
	}

	@:hlNative("std","type_enum_eq")
	public static function enumEq<T:EnumValue>( a : T, b : T ) : Bool {
		return false;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		var en : hl.BaseType.Enum = cast getEnum(e);
		return en.__constructs__[Type.enumIndex(e)];
	}

	@:hlNative("std","enum_parameters")
	static function _enumParameters( e : EnumValue ) : hl.NativeArray<Dynamic> {
		return null;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		var arr = _enumParameters(e);
		return cast hl.types.ArrayObj.alloc(arr);
	}

	@:extern public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped $enumIndex(e);
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		var en : hl.BaseType.Enum = cast e;
		var out = [];
		for( i in 0...en.__evalues__.length ) {
			var v = en.__evalues__[i];
			if( v != null ) out.push(v);
		}
		return out;
	}

}