import cs.Lib;
import cs.internal.HxObject;
import cs.internal.Runtime;
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

@:keep enum ValueType {
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

@:keep @:core_api class Type {
	
	@:functionBody('
		if (o is haxe.lang.DynamicObject || o is System.Type)
			return null;
		
		return o.GetType();
	')
	public static function getClass<T>( o : T ) : Class<T> untyped 
	{
		return null;
	}
	
	@:functionBody('
		if (o is System.Enum || o is haxe.lang.Enum)
			return o.GetType();
		return null;
	')
	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped 
	{
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> 
	{
		var t:cs.system.Type = Lib.toNativeType(c);
		var base = t.BaseType;
		if (base == null || (base + "") == ("haxe.lang.HxObject") || (base + "") == ("System.Object"))
		{
			return null;
		}
		
		return Lib.fromNativeType(base);
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		var ret:String = cast Lib.toNativeType(c);
#if no-root
		if (ret.length > 10 && StringTools.startsWith(ret, "haxe.root."))
			ret = ret.substr(10);
#end
		
		return switch(ret)
		{
			case "System.Int32": "Int";
			case "System.Double": "Float";
			case "System.String": "String";
			case "System.Object": "Dynamic";
			case "System.Type": "Class";
			default: ret.split("`")[0];
		}
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String 
	{
		var ret:String = cast Lib.toNativeType(untyped e);
#if no-root
		if (ret.length > 10 && StringTools.startsWith(ret, "haxe.root."))
			ret = ret.substr(10);
#end
		if (ret.length == 14 && ret == "System.Boolean") 
			return "Bool";
		return ret;
	}
	
	public static function resolveClass( name : String ) : Class<Dynamic> 
	{
#if no-root
		if (name.indexOf(".") == -1)
			name = "haxe.root." + name;
#end
		var t:cs.system.Type = cs.system.Type.GetType(name);
		if (t == null)
		{
			switch(name)
			{
				case #if no-root "haxe.root.Int" #else "Int" #end: return Int;
				case #if no-root "haxe.root.Float" #else "Float" #end: return Float;
				case #if no-root "haxe.root.Class" #else "Class" #end: return Class;
				case #if no-root "haxe.root.String" #else "String" #end: return String;
				case #if no-root "haxe.root.Dynamic" #else "Dynamic" #end: return Dynamic;
				default: return null;
			}
		} else if (t.IsInterface && cast(untyped __typeof__(IGenericObject), cs.system.Type).IsAssignableFrom(t)) { 
			t = null;
			var i = 0;
			var ts = "";
			while (t == null && i < 18)
			{
				i++;
				ts += (i == 1 ? "" : ",") + "System.Object";
				t = cs.system.Type.GetType(name + "`" + i + "[" + ts + "]");
			}
			
			return Lib.fromNativeType(t);
		} else {
			return Lib.fromNativeType(t);
		}
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped
	{
		if (name == "Bool") return Bool;
		return cast resolveClass(name);
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T
	{
		var t:cs.system.Type = Lib.toNativeType(cl);
		var ctors = t.GetConstructors();
		return Runtime.callMethod(null, cast ctors, ctors.Length, args);
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T 
	{
		if (Reflect.hasField(cl, "__hx_createEmpty"))
			return untyped cl.__hx_createEmpty();
		return createInstance(cl, []);
	}
	
	@:functionBody('
		if (@params == null) 
		{
			object ret = haxe.lang.Runtime.slowGetField(e, constr, false);
			if (ret is haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + constr + " needs parameters");
			return (T) ret;
		} else {
			return (T) haxe.lang.Runtime.slowCallField(e, constr, @params);
		}
	')
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T 
	{
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var constr = getEnumConstructs(e);
		return createEnum(e, constr[index], params);
	}
	
	@:functionBody('
		if (c == typeof(string))
		{
			return haxe.lang.StringRefl.fields;
		}
		
		Array<object> ret = new Array<object>();

        System.Reflection.MemberInfo[] mis = c.GetMembers(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.DeclaredOnly | System.Reflection.BindingFlags.Instance);
        for (int i = 0; i < mis.Length; i++)
        {
			if (mis[i] is System.Reflection.PropertyInfo)
                continue;
			string n = mis[i].Name;
			if (!n.StartsWith("__hx_") && n[0] != \'.\')
				ret.push(mis[i].Name);
        }

		return ret;
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}
	
	@:functionBody('
		Array<object> ret = new Array<object>();
		
		if (c == typeof(string))
		{
			ret.push("fromCharCode");
			return ret;
		}

        System.Reflection.MemberInfo[] mis = c.GetMembers(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static);
        for (int i = 0; i < mis.Length; i++)
        {
            string n = mis[i].Name;
			if (!n.StartsWith("__hx_"))
				ret.push(mis[i].Name);
        }

        return ret;
	')
	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		if (Reflect.hasField(e, "constructs"))
			return untyped e.constructs;
		return untyped __cs__("new Array<object>(System.Enum.GetNames(e))");
	}
	
	@:functionBody('
		if (v == null) return ValueType.TNull;

        System.Type t = v as System.Type;
        if (t != null)
        {
            //class type
            return ValueType.TObject;
        }

        t = v.GetType();
        if (t.IsEnum) 
            return ValueType.TEnum(t);
        if (t.IsValueType)
        {
            System.IConvertible vc = v as System.IConvertible;
            if (vc != null)
            {
                switch (vc.GetTypeCode())
                {
                    case System.TypeCode.Boolean: return ValueType.TBool;
                    case System.TypeCode.Double:
						double d = vc.ToDouble(null);
						if (d >= int.MinValue && d <= int.MaxValue && d == vc.ToInt32(null))
							return ValueType.TInt;
						else
							return ValueType.TFloat;
                    case System.TypeCode.Int32:
                        return ValueType.TInt;
                    default:
                        return ValueType.TClass(t);
                }
            } else {
                return ValueType.TClass(t);
            }
        }

        if (v is haxe.lang.IHxObject)
        {
            if (v is haxe.lang.DynamicObject)
                return ValueType.TObject;
            else if (v is haxe.lang.Enum)
                return ValueType.TEnum(t);
            return ValueType.TClass(t);
        } else if (v is haxe.lang.Function) {
            return ValueType.TFunction;
        } else {
            return ValueType.TClass(t);
        }
	')
	public static function typeof( v : Dynamic ) : ValueType 
	{
		return null;
	}
	
	@:functionBody('
			if (a is haxe.lang.Enum)
				return a.Equals(b);
			else
				return haxe.lang.Runtime.eq(a, b);
	')
	public static function enumEq<T>( a : T, b : T ) : Bool 
	{
		return untyped a.Equals(b);
	}
	
	@:functionBody('
		if (e is System.Enum)
			return e + "";
		else
			return ((haxe.lang.Enum) e).getTag();
	')
	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return e.tag;
	}
	
	@:functionBody('
		return ( e is System.Enum ) ? new Array<object>() : ((haxe.lang.Enum) e).@params;
	')
	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return null;
	}
	
	@:functionBody('
		if (e is System.Enum)
			return ((System.IConvertible) e).ToInt32(null);
		else
			return ((haxe.lang.Enum) e).index;
	')
	public static function enumIndex( e : EnumValue ) : Int  untyped
	{
		return e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> 
	{
		return null;
	}

}

