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
		if (o instanceof java.lang.Enum || o instanceof haxe.lang.Enum)
			return null;
		
		java.lang.Class<T> ret = (java.lang.Class<T>) o.getClass();
		String name = ret.getName();
		if (name == "java.lang.Object" || name == "haxe.lang.Dynamic" || name == "java.lang.Class")
			return null;
		return ret;
	')
	public static function getClass<T>( o : T ) : Class<T> untyped 
	{
		return null;
	}
	
	@:functionBody('
		if (o instanceof java.lang.Enum || o instanceof haxe.lang.Enum)
			return o.getClass();
		return null;
	')
	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped 
	{
		return null;
	}
	
	@:functionBody('
		return (c == null) ? null : c.getSuperclass();
	')
	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped 
	{
		return null;
	}
	
	public static function getClassName( c : Class<Dynamic> ) : String untyped {
		var name:String = c.getName();
		if (name.startsWith("haxe.root."))
			return name.substr(10);
			
		return switch(name)
		{
			case "int", "java.lang.Integer": "Int";
			case "double", "java.lang.Double": "Float";
			case "java.lang.String": "String";
			case "boolean", "java.lang.Boolean": "Bool";
			default: name;
		}
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String untyped {
		var ret:String = e.getName();
		if (ret.startsWith("haxe.root."))
			return ret.substr(10);
			
		return ret;
	}

	@:functionBody('
		try {
			if (name.indexOf(".") == -1)
				name = "haxe.root." + name;
			return java.lang.Class.forName(name);
		}
		catch (java.lang.ClassNotFoundException e)
		{
			return null;
		}
	')
	public static function resolveClass( name : String ) : Class<Dynamic> untyped 
	{
		return null;
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped 
	{
		return resolveClass(name);
	}

	@:functionBody('
		try {
			int len = args.length;
			java.lang.Class[] cls = new java.lang.Class[len];
			java.lang.Object[] objs = new java.lang.Object[len];
			
			java.lang.reflect.Constructor[] ms = cl.getConstructors();
			int msl = ms.length;
			int lstRes = 0;
			int realMsl = 0;
			for(int i =0; i < msl; i++)
			{
				if (!ms[i].isVarArgs() && ms[i].getParameterTypes().length != len)
				{
					ms[i] = null;
				} else {
					ms[lstRes] = ms[i];
					if (lstRes != i)
						ms[i] = null;
					lstRes++;
					realMsl++;
				}
			}
			
			boolean hasNumber = false;
			
			for (int i = 0; i < len; i++)
			{
				Object o = args.__get(i);
				objs[i]= o;
				cls[i] = o.getClass();
				
				if (!(o instanceof java.lang.Number))
				{
					lstRes = 0;
					msl = realMsl;
					
					for (int j = 0; j < msl; j++)
					{
						java.lang.Class[] allcls = ms[j].getParameterTypes();
						if (i < allcls.length)
						{
							if (!allcls[i].isAssignableFrom(cls[i]))
							{
								ms[j] = null;
							} else {
								ms[lstRes] = ms[j];
								if (lstRes != j)
									ms[j] = null;
								lstRes++;
								realMsl++;
							}
						}
					}
					
					if (realMsl == 0)
						return null;
				} else {
					hasNumber = true;
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
			
			found.setAccessible(true);
			return (T) found.newInstance(objs);
		} catch(Throwable t) {
			return null;
		}
	')
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped 
	{
		return null;
	}
	
	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped 
	{
		if (Reflect.hasField(cl, "__hx_createEmpty"))
			return cl.__hx_createEmpty();
		return createInstance(cl, []);
	}
	
	@:functionBody('
		if (params == null) 
		{
			T ret = (T) haxe.lang.Runtime.slowGetField(e, constr, false);
			if (ret instanceof haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + constr + " needs parameters");
			return ret;
		} else {
			return (T) haxe.lang.Runtime.slowCallField(e, constr, params);
		}
	')
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T 
	{
		return null;
	}
	
	@:functionBody('
		/*if (params == null) {
			T ret = (T) e.__hx_getField(index + "", false, false, false);
			if (ret instanceof haxe.lang.Function)
				throw haxe.lang.HaxeException.wrap("Constructor " + index + " needs parameters");
			return ret;
		} else {
			return (T)e.__hx_invokeField(index + "", false, params);
		}*/
		return null; //TODO
	')
	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		return null;
	}
	
	@:functionBody('
		Array<String> ret = new Array<String>();
		for (java.lang.reflect.Field f : c.getDeclaredFields())
		{
			java.lang.String fname = f.getName();
			if (!java.lang.reflect.Modifier.isStatic(f.getModifiers()) && !fname.startsWith("__hx_"))
				ret.push(fname);
		}
		
		for (java.lang.reflect.Method m : c.getDeclaredMethods())
		{
			java.lang.String mname = m.getName();
			if (!java.lang.reflect.Modifier.isStatic(m.getModifiers()) && !mname.startsWith("__hx_"))
				ret.push(mname);
		}
		
		return ret;
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}
	
	@:functionBody('
		Array<String> ret = new Array<String>();
		for (java.lang.reflect.Field f : c.getDeclaredFields())
		{
			java.lang.String fname = f.getName();
			if (java.lang.reflect.Modifier.isStatic(f.getModifiers()) && !fname.startsWith("__hx_"))
			ret.push(fname);
		}
		
		for (java.lang.reflect.Method m : c.getDeclaredMethods())
		{
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
		if (Reflect.hasField(e, "constructs"))
			return untyped e.constructs;
		return getClassFields(cast e);
	}
	
	@:functionBody('
		if (v == null) return ValueType.TNull;
		
		if (v instanceof haxe.lang.IHxObject) {
			haxe.lang.IHxObject vobj = (haxe.lang.IHxObject) v;
			java.lang.Class cl = vobj.getClass();
			if (v instanceof haxe.lang.DynamicObject)
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

	public static function enumEq<T>( a : T, b : T ) : Bool untyped 
	{
		return a.equals(b);
	}

	@:functionBody('
		if (e instanceof java.lang.Enum)
			return ((java.lang.Enum) e).name();
		else
			return ((haxe.lang.Enum) e).getTag();
	')
	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return null;
	}

	@:functionBody('
		return ( e instanceof java.lang.Enum ) ? new haxe.root.Array() : ((haxe.lang.Enum) e).params;
	')
	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return null;
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