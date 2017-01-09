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
import cs.Lib;
import cs.internal.HxObject;
import cs.internal.Runtime;
import cs.internal.Function;
import cs.Flags;
import cs.system.Object;
import cs.system.reflection.*;
using StringTools;

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

@:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T> untyped
	{
		if (Object.ReferenceEquals(o,null) || Std.is(o,DynamicObject) || Std.is(o,cs.system.Type))
			return null;

		return untyped o.GetType();
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped
	{
		if (Std.is(o, cs.system.Enum) || Std.is(o,HxEnum))
			return untyped o.GetType();
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>
	{
		var t:cs.system.Type = Lib.toNativeType(c);
		var base = t.BaseType;
		if (base == null || base.ToString() == "haxe.lang.HxObject" || base.ToString() == "System.Object")
			return null;
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
		var t:cs.system.Type = cs.system.Type._GetType(name);
#if !CF
		if (t == null)
		{
			var all = cs.system.AppDomain.CurrentDomain.GetAssemblies().GetEnumerator();
			while (all.MoveNext())
			{
				var t2:cs.system.reflection.Assembly = all.Current;
				t = t2.GetType(name);
				if (t != null)
					break;
			}
		}
#end
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
#if !erase_generics
		} else if (t.IsInterface && cast(untyped __typeof__(IGenericObject), cs.system.Type).IsAssignableFrom(t)) {
			for (attr in t.GetCustomAttributes(true))
			{
				var g = cs.Lib.as(attr, cs.internal.HxObject.GenericInterface);
				if (g != null)
					return Lib.fromNativeType(g.generic);
			}

			return Lib.fromNativeType(t);
#end
		} else {
			return Lib.fromNativeType(t);
		}
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped
	{
		if (name == "Bool") return Bool;
		var t = Lib.toNativeType(resolveClass(name));
		if (t != null && t.BaseType.Equals( Lib.toNativeType(cs.system.Enum) ) || Lib.toNativeType(HxEnum).IsAssignableFrom(t))
			return t;
		return null;
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

	// cache empty constructor arguments so we don't allocate it on each createEmptyInstance call
	@:protected @:readOnly static var __createEmptyInstance_EMPTY_ARGS = cs.NativeArray.make((cs.internal.Runtime.EmptyObject.EMPTY : Any));

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		var t = Lib.toNativeType(cl);

		if (cs.system.Object.ReferenceEquals(t, String))
			#if erase_generics
			return untyped "";
			#else
			return untyped __cs__("(T)(object){0}", "");
			#end

		var res = try
			cs.system.Activator.CreateInstance(t, __createEmptyInstance_EMPTY_ARGS)
		catch (_:cs.system.MissingMemberException)
			cs.system.Activator.CreateInstance(t);

		#if erase_generics
		return res;
		#else
		return untyped __cs__("(T){0}", res);
		#end
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T
	{
		if (params == null || params.length == 0)
		{
			var ret = cs.internal.Runtime.slowGetField(e, constr, true);
			if (Reflect.isFunction(ret))
				throw 'Constructor $constr needs parameters';
			return ret;
		} else {
			return cs.internal.Runtime.slowCallField(e,constr,params);
		}
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var constr = getEnumConstructs(e);
		return createEnum(e, constr[index], params);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String>
	{
		if (c == String)
			return cs.internal.StringExt.StringRefl.fields;

		var c = cs.Lib.toNativeType(c);
		var ret = [];
		var mis = c.GetMembers(new cs.Flags(BindingFlags.Public) | BindingFlags.Instance | BindingFlags.FlattenHierarchy);
		for (i in 0...mis.Length)
		{
			var i = mis[i];
			if (Std.is(i, PropertyInfo))
				continue;
			var n = i.Name;
			if (!n.startsWith('__hx_') && n.fastCodeAt(0) != '.'.code)
			{
				switch(n)
				{
					case 'Equals' | 'ToString' | 'GetHashCode' | 'GetType':
					case _:
						ret.push(n);
				}
			}
		}
		return ret;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		if (Object.ReferenceEquals(c, String))
		{
			return ['fromCharCode'];
		}

		var ret = [];
		var infos = Lib.toNativeType(c).GetMembers(new Flags(BindingFlags.Public) | BindingFlags.Static);
		for (i in 0...infos.Length)
		{
			var name = infos[i].Name;
			if (!name.startsWith('__hx_'))
			{
				ret.push(name);
			}
		}
		return ret;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		if (Reflect.hasField(e, "__hx_constructs")) {
			var ret:Array<String> = cs.Lib.array(untyped e.__hx_constructs);
			return ret.copy();
		}
		return cs.Lib.array(cs.system.Enum.GetNames(untyped e));
	}

	public static function typeof( v : Dynamic ) : ValueType {
		if (v == null) return ValueType.TNull;

		var t:cs.system.Type = cs.Lib.as(v, cs.system.Type);
		if (t != null) {
			//class type
			return ValueType.TObject;
		}

		t = v.GetType();
		if (t.IsEnum || Std.is(v, HxEnum))
			return ValueType.TEnum(cast t);
		if (t.IsValueType) {
			var vc:cs.system.IConvertible = cast v;
			if (vc != null) {
				switch (vc.GetTypeCode()) {
					case cs.system.TypeCode.Boolean: return ValueType.TBool;
					case cs.system.TypeCode.Double:
						var d:Float = vc.ToDouble(null);
						if (d >= cs.system.Int32.MinValue && d <= cs.system.Int32.MaxValue && d == vc.ToInt32(null))
							return ValueType.TInt;
						else
							return ValueType.TFloat;
					case cs.system.TypeCode.Int32:
						return ValueType.TInt;
					default:
						return ValueType.TClass(cast t);
				}
			} else {
				return ValueType.TClass(cast t);
			}
		}

		if (Std.is(v, IHxObject)) {
			if (Std.is(v, DynamicObject))
				return ValueType.TObject;
			else if (Std.is(v, HxEnum))
				return ValueType.TEnum(cast t);
			return ValueType.TClass(cast t);
		} else if (Std.is(v, Function)) {
			return ValueType.TFunction;
		} else {
			return ValueType.TClass(cast t);
		}
	}

	public static function enumEq<T>( a : T, b : T ) : Bool
	{
		if (a == null)
			return b == null;
		else if (b == null)
			return false;
		else
			return untyped a.Equals(b);
	}

	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return Std.is(e, cs.system.Enum) ? e+'' : cast(e,HxEnum).getTag();
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return Std.is(e, cs.system.Enum) ? [] : cast(e,HxEnum).getParams();
	}

	@:pure
	public static function enumIndex( e : EnumValue ) : Int  untyped
	{
		if (Std.is(e, cs.system.Enum))
		{
			var values = cs.system.Enum.GetValues(Lib.getNativeType(e));
			return cs.system.Array.IndexOf(values, e);
		} else {
			return cast(e, HxEnum).index;
		}
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

