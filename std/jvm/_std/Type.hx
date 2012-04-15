/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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

@:core_api class Type {
	
	@:functionBody('
		if (o instanceof haxe.lang.IHxObject)
		{
			return ((haxe.lang.IHxObject) o).__hx_getClass();
		} else {
			//TODO implement slow method
			return null;
		}
	')
	public static function getClass<T>( o : T ) : Class<T> untyped 
	{
		return null;
	}
	
	@:functionBody('
		if (o instanceof haxe.lang.IHxObject)
		{
			return ((haxe.lang.IHxObject) o).__hx_getClass();
		} else {
			//TODO implement slow method
			return null;
		}
	')
	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped 
	{
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped 
	{
		return null;
	}
	
	public static function getClassName( c : Class<Dynamic> ) : String untyped {
		var name:String = cast(c.nativeType(), jvm.native.lang.Class<Dynamic>).getName();
		if (name.startsWith("haxe.root."))
			return name.substr(10);
			
		return switch(name)
		{
			case "int": "Int";
			case "double": "Float";
			case "java.lang.String": "String";
			case "boolean": "Bool";
			default: name;
		}
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String untyped {
		return cast(e.nativeType(), jvm.native.lang.Class<Dynamic>).getName();
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped 
	{
		return null;
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped 
	{
		return null;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped 
	{
		return cl.__hx_create(args);
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped 
	{
		return cl.__hx_createEmpty();
	}
	
	@:functionBody('
		if (params == null) {
			T ret = (T) e.__hx_getField(constr, false, false, false);
			if (ret instanceof haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + constr + " needs parameters");
			return ret;
		} else {
			return (T)e.__hx_invokeField(constr, false, params);
		}
	')
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T 
	{
		return null;
	}
	
	@:functionBody('
		if (params == null) {
			T ret = (T) e.__hx_getField(index + "", false, false, false);
			if (ret instanceof haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + index + " needs parameters");
			return ret;
		} else {
			return (T)e.__hx_invokeField(index + "", false, params);
		}
	')
	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		return null;
	}

	static function describe( t : Dynamic, fact : Bool ) : Array<String> untyped {
		return null;
	}
	
	@:functionBody('
		if (c instanceof haxe.lang.IHxObject)
		{
			Array<String> ret = new Array<String>();
			((haxe.lang.IHxObject) c).__hx_getFields(ret, true);
			return ret;
		} else {
			return null;
		}
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		return null;
	}
	
	@:functionBody('
		if (v == null) return ValueType.TNull;
		
		if (v instanceof haxe.lang.IHxObject) {
			haxe.lang.IHxObject vobj = (haxe.lang.IHxObject) v;
			haxe.lang.Class cl = vobj.__hx_getClass();
			if (cl == null)
				return ValueType.TObject;
			else if (v instanceof haxe.lang.Enum)
				return ValueType.TEnum(cl);
			else
				return ValueType.TClass(cl);
		} else if (v instanceof java.lang.Number) {
			java.lang.Number n = (java.lang.Number) v;
			if (n.intValue() == n.doubleValue())
				return ValueType.TInt;
			else
				return ValueType.TFloat;
		} else if (v instanceof haxe.lang.Function) {
			return ValueType.TFunction;
		} else if (v instanceof java.lang.Enum) {
			return ValueType.TEnum(new haxe.lang.NativeClassWrapper(v.getClass()));
		} else if (v instanceof java.lang.Boolean) {
			return ValueType.TBool;
		} else {
			return ValueType.TClass(new haxe.lang.NativeClassWrapper(v.getClass()));
		}
	')
	public static function typeof( v : Dynamic ) : ValueType untyped 
	{
		return null;
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped 
	{
		return a.equals(b);
	}

	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return e.tag;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return if( e.params == null ) [] else e.params;
	}
	
	@:functionBody('
		if (e instanceof java.lang.Enum)
			return ((java.lang.Enum) e).ordinal();
		else
			return ((haxe.lang.Enum) e).index;
	')
	public static function enumIndex( e : EnumValue ) : Int untyped
	{
		return e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> 
	{
		return null;
	}

}

