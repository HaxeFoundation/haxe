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

import java.internal.HxObject;

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

	public static function getClass<T>( o : T ) : Class<T>
	{
		if (o == null || Std.is(o, DynamicObject) || Std.is(o, java.lang.Class)) {
			return null;
		}
		return cast java.Lib.getNativeType(o);
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic>
	{
		if (Std.is(o, java.lang.Enum) || Std.is(o, HxEnum)) {
			return untyped o.getClass();
		}
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>
	{
		var c = java.Lib.toNativeType(c);
		var cl:java.lang.Class<Dynamic> = c == null ? null : untyped c.getSuperclass();
		if (cl != null && cl.getName() != "haxe.lang.HxObject" && cl.getName() != "java.lang.Object") {
			return cast cl;
		}
		return null;
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		var c : java.lang.Class<Dynamic> = cast c;
		var name:String = c.getName();
		if (name.startsWith("haxe.root."))
			return name.substr(10);
		if (name.startsWith("java.lang"))
			name = name.substr(10);

		return switch(name)
		{
			case "int", "Integer": "Int";
			case "double", "Double": "Float";
			case "Object": "Dynamic";
			default: name;
		}
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		var c : java.lang.Class<Dynamic> = cast e;
		var ret:String = c.getName();
		if (ret.startsWith("haxe.root."))
			return ret.substr(10);
		else if (ret == "boolean" || ret == "java.lang.Boolean")
			return "Bool";

		return ret;
	}

	public static function resolveClass( name : String ) : Class<Dynamic>
	{
		try {
			if (name.indexOf(".") == -1) {
				name = "haxe.root." +name;
			}
			return cast java.lang.Class.forName(name);
		} catch (e:java.lang.ClassNotFoundException) {
			return untyped switch (name) {
				case "haxe.root.Int": Int;
				case "haxe.root.Float": Float;
				case "haxe.root.String": String;
				case "haxe.root.Math": java.lang.Math;
				case "haxe.root.Class": java.lang.Class;
				case "haxe.root.Dynamic": java.lang.Object;
				case _: null;
			}
		}
	}


	@:functionCode('
		if ("Bool".equals(name)) return boolean.class;
		Class r = resolveClass(name);
		if (r != null && (r.getSuperclass() == java.lang.Enum.class || haxe.lang.Enum.class.isAssignableFrom(r)))
			return r;
		return null;
	')
	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped
	{
		if (name == "Bool") return Bool;
		return resolveClass(name);
	}

	@:functionCode('
			int len = args.length;
			java.lang.Class[] cls = new java.lang.Class[len];
			java.lang.Object[] objs = new java.lang.Object[len];

			java.lang.reflect.Constructor[] ms = cl.getConstructors();
			int msl = ms.length;
			int realMsl = 0;
			for(int i =0; i < msl; i++)
			{
				if (!ms[i].isVarArgs() && ms[i].getParameterTypes().length != len)
				{
					ms[i] = null;
				} else {
					ms[realMsl] = ms[i];
					if (realMsl != i)
						ms[i] = null;
					realMsl++;
				}
			}

			boolean hasNumber = false;

			for (int i = 0; i < len; i++)
			{
				Object o = args.__get(i);
				objs[i]= o;
				cls[i] = o.getClass();
				boolean isNum = false;

				if (o instanceof java.lang.Number)
				{
					cls[i] = java.lang.Number.class;
					isNum = hasNumber = true;
				}

				msl = realMsl;
				realMsl = 0;

				for (int j = 0; j < msl; j++)
				{
					java.lang.Class[] allcls = ms[j].getParameterTypes();
					if (i < allcls.length)
					{
						if (! ((isNum && allcls[i].isPrimitive()) || allcls[i].isAssignableFrom(cls[i])) )
						{
							ms[j] = null;
						} else {
							ms[realMsl] = ms[j];
							if (realMsl != j)
								ms[j] = null;
							realMsl++;
						}
					}
				}

			}

			java.lang.reflect.Constructor found = ms[0];

			if (hasNumber)
			{
				java.lang.Class[] allcls = found.getParameterTypes();

				for (int i = 0; i < len; i++)
				{
					java.lang.Object o = objs[i];
					if (o instanceof java.lang.Number)
					{
						java.lang.Class curCls = null;
						if (i < allcls.length)
						{
							curCls = allcls[i];
							if (!curCls.isAssignableFrom(o.getClass()))
							{
								String name = curCls.getName();
								if (name.equals("double") || name.equals("java.lang.Double"))
								{
									objs[i] = ((java.lang.Number)o).doubleValue();
								} else if (name.equals("int") || name.equals("java.lang.Integer"))
								{
									objs[i] = ((java.lang.Number)o).intValue();
								} else if (name.equals("float") || name.equals("java.lang.Float"))
								{
									objs[i] = ((java.lang.Number)o).floatValue();
								} else if (name.equals("byte") || name.equals("java.lang.Byte"))
								{
									objs[i] = ((java.lang.Number)o).byteValue();
								} else if (name.equals("short") || name.equals("java.lang.Short"))
								{
									objs[i] = ((java.lang.Number)o).shortValue();
								}
							}
						} //else varargs not handled TODO
					}
				}
			}

		try {
			found.setAccessible(true);
			return (T) found.newInstance(objs);
		}
		catch (java.lang.reflect.InvocationTargetException e)
		{
			throw haxe.lang.HaxeException.wrap(e.getCause());
		}

		catch (Throwable t)
		{
			throw haxe.lang.HaxeException.wrap(t);
		}
	')
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped
	{
		return null;
	}

	// cache empty constructor arguments so we don't allocate it on each createEmptyInstance call
	@:protected @:readOnly static var __createEmptyInstance_EMPTY_TYPES = java.NativeArray.make(java.Lib.toNativeEnum(java.internal.Runtime.EmptyObject));
	@:protected @:readOnly static var __createEmptyInstance_EMPTY_ARGS = java.NativeArray.make(java.internal.Runtime.EmptyObject.EMPTY);

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		var t = java.Lib.toNativeType(cl);
		try {
			var ctor = t.getConstructor(__createEmptyInstance_EMPTY_TYPES);
			return ctor.newInstance(__createEmptyInstance_EMPTY_ARGS);
		} catch (_:java.lang.NoSuchMethodException) {
			return t.newInstance();
		}
	}

	@:functionCode('
		if (params == null || params.length == 0)
		{
			java.lang.Object ret = haxe.lang.Runtime.slowGetField(e, constr, true);
			if (ret instanceof haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + constr + " needs parameters");
			return (T) ret;
		} else {
			return (T) haxe.lang.Runtime.slowCallField(e, constr, params);
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
		if (c == java.lang.String.class)
		{
			return haxe.lang.StringRefl.fields;
		}

		Array<String> ret = new Array<String>();
		for (java.lang.reflect.Field f : c.getFields())
		{
			java.lang.String fname = f.getName();
			if (!java.lang.reflect.Modifier.isStatic(f.getModifiers()) && !fname.startsWith("__hx_"))
				ret.push(fname);
		}

		for (java.lang.reflect.Method m : c.getMethods())
		{
			if (m.getDeclaringClass() == java.lang.Object.class)
				continue;
			java.lang.String mname = m.getName();
			if (!java.lang.reflect.Modifier.isStatic(m.getModifiers()) && !mname.startsWith("__hx_"))
				ret.push(mname);
		}

		return ret;
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	@:functionCode('
		Array<String> ret = new Array<String>();
		if (c == java.lang.String.class)
		{
			ret.push("fromCharCode");
			return ret;
		}

		for (java.lang.reflect.Field f : c.getDeclaredFields())
		{
			java.lang.String fname = f.getName();
			if (java.lang.reflect.Modifier.isStatic(f.getModifiers()) && !fname.startsWith("__hx_"))
			ret.push(fname);
		}

		for (java.lang.reflect.Method m : c.getDeclaredMethods())
		{
			if (m.getDeclaringClass() == java.lang.Object.class)
				continue;
			java.lang.String mname = m.getName();
			if (java.lang.reflect.Modifier.isStatic(m.getModifiers()) && !mname.startsWith("__hx_"))
				ret.push(mname);
		}

		return ret;
	')
	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		if (Reflect.hasField(e, "__hx_constructs"))
		{
			var ret:Array<String> = java.Lib.array(untyped e.__hx_constructs);
			return ret.copy();
		}
		var vals:java.NativeArray<java.lang.Enum<Dynamic>> = untyped e.values(), ret = [];
		for (i in 0...vals.length)
			ret[i] = vals[i].name();
		return ret;
	}

	@:functionCode('
		if (v == null) return ValueType.TNull;

		if (v instanceof haxe.lang.IHxObject) {
			haxe.lang.IHxObject vobj = (haxe.lang.IHxObject) v;
			java.lang.Class cl = vobj.getClass();
			if (v instanceof haxe.lang.DynamicObject)
				return ValueType.TObject;
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
		} else if (v instanceof java.lang.Enum || v instanceof haxe.lang.Enum) {
			return ValueType.TEnum(v.getClass());
		} else if (v instanceof java.lang.Boolean) {
			return ValueType.TBool;
		} else if (v instanceof java.lang.Class) {
			return ValueType.TObject;
		} else {
			return ValueType.TClass(v.getClass());
		}
	')
	public static function typeof( v : Dynamic ) : ValueType untyped
	{
		return null;
	}

	@:functionCode('
			if (a instanceof haxe.lang.Enum)
				return a.equals(b);
			else
				return haxe.lang.Runtime.eq(a, b);
	')
	public static function enumEq<T>( a : T, b : T ) : Bool untyped
	{
		return a == b;
	}

	@:functionCode('
		if (e instanceof java.lang.Enum)
			return ((java.lang.Enum) e).name();
		else
			return ((haxe.lang.Enum) e).getTag();
	')
	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return null;
	}

	@:functionCode('
		return ( e instanceof java.lang.Enum ) ? new haxe.root.Array() : ((haxe.lang.Enum) e).getParams();
	')
	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return null;
	}

	@:functionCode('
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
