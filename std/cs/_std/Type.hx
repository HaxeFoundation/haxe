/*
 * Copyright (C)2005-2012 Haxe Foundation
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

@:keep @:coreApi class Type {

	@:functionCode('
		if (o == null || o is haxe.lang.DynamicObject || o is System.Type)
			return null;

		return o.GetType();
	')
	public static function getClass<T>( o : T ) : Class<T> untyped
	{
		return null;
	}

	@:functionCode('
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
#if no_root
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
#if no_root
		if (ret.length > 10 && StringTools.startsWith(ret, "haxe.root."))
			ret = ret.substr(10);
#end
		if (ret.length == 14 && ret == "System.Boolean")
			return "Bool";
		return ret;
	}

	public static function resolveClass( name : String ) : Class<Dynamic>
	{
#if no_root
		if (name.indexOf(".") == -1)
			name = "haxe.root." + name;
#end
		var t:cs.system.Type = cs.system.Type.GetType(name);
		if (t == null)
		{
			switch(name)
			{
				case #if no_root "haxe.root.Int" #else "Int" #end: return cast Int;
				case #if no_root "haxe.root.Float" #else "Float" #end: return cast Float;
				case #if no_root "haxe.root.Class" #else "Class" #end: return cast Class;
				case #if no_root "haxe.root.Dynamic" #else "Dynamic" #end: return cast Dynamic;
				case #if no_root "haxe.root.String" #else "String" #end: return cast String;
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

	@:functionCode('
		if (name == "Bool") return typeof(bool);
		System.Type t = resolveClass(name);
		if (t != null && (t.BaseType.Equals(typeof(System.Enum)) || t.BaseType.Equals(typeof(haxe.lang.Enum))))
			return t;
		return null;
	')
	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped
	{
		if (name == "Bool") return Bool;
		return cast resolveClass(name);
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T
	{
		if (untyped cl == String)
			return args[0];
		var t:cs.system.Type = Lib.toNativeType(cl);
		if (t.IsInterface)
		{
			//may be generic
			t = Lib.toNativeType(resolveClass(getClassName(cl)));
		}
		var ctors = t.GetConstructors();
		return Runtime.callMethod(null, cast ctors, ctors.Length, args);
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T
	{
		var t:cs.system.Type = Lib.toNativeType(cl);
		if (t.IsInterface)
		{
			//may be generic
			t = Lib.toNativeType(resolveClass(getClassName(cl)));
		}

		if (Reflect.hasField(cl, "__hx_createEmpty"))
			return untyped cl.__hx_createEmpty();
		return createInstance(cl, []);
	}

	@:functionCode('
		if (@params == null || @params[0] == null)
		{
			object ret = haxe.lang.Runtime.slowGetField(e, constr, true);
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

	@:functionCode('
		if (c == typeof(string))
		{
			return haxe.lang.StringRefl.fields;
		}

		Array<object> ret = new Array<object>();

        System.Reflection.MemberInfo[] mis = c.GetMembers(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.FlattenHierarchy);
        for (int i = 0; i < mis.Length; i++)
        {
			if (mis[i] is System.Reflection.PropertyInfo)
                continue;
			string n = mis[i].Name;
			if (!n.StartsWith("__hx_") && n[0] != \'.\' && !n.Equals("Equals") && !n.Equals("ToString") && !n.Equals("GetHashCode") && !n.Equals("GetType"))
				ret.push(mis[i].Name);
        }

		return ret;
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	@:functionCode('
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
			return untyped e.constructs.copy();
		return untyped __cs__("new Array<object>(System.Enum.GetNames(e))");
	}

	@:functionCode('
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

	@:functionCode('
			if (a is haxe.lang.Enum)
				return a.Equals(b);
			else
				return haxe.lang.Runtime.eq(a, b);
	')
	public static function enumEq<T>( a : T, b : T ) : Bool
	{
		return untyped a.Equals(b);
	}

	@:functionCode('
		if (e is System.Enum)
			return e + "";
		else
			return ((haxe.lang.Enum) e).getTag();
	')
	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return e.tag;
	}

	@:functionCode('
		return ( e is System.Enum ) ? new Array<object>() : ((haxe.lang.Enum) e).@params;
	')
	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return null;
	}

	@:functionCode('
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
		var ctors = getEnumConstructs(e);
		var ret = [];
		for (ctor in ctors)
		{
			var v = Reflect.field(e, ctor);
			if (Std.is(v, e))
				ret.push(v);
		}

		return ret;
	}

}

