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
package java.internal;

/**
 This class is meant for internal compiler use only. It provides the Haxe runtime
 compatibility to the host language. Do not access it directly.
**/

@:native('haxe.lang.Runtime')
@:nativeGen
@:classCode('
	public static java.lang.Object getField(haxe.lang.IHxObject obj, java.lang.String field, boolean throwErrors)
	{
		if (obj == null && !throwErrors) return null;
		return obj.__hx_getField(field, throwErrors, false, false);
	}

	public static double getField_f(haxe.lang.IHxObject obj, java.lang.String field, boolean throwErrors)
	{
		if (obj == null && !throwErrors) return 0.0;
		return obj.__hx_getField_f(field, throwErrors, false);
	}

	public static java.lang.Object setField(haxe.lang.IHxObject obj, java.lang.String field, java.lang.Object value)
	{
		return obj.__hx_setField(field, value, false);
	}

	public static double setField_f(haxe.lang.IHxObject obj, java.lang.String field, double value)
	{
		return obj.__hx_setField_f(field, value, false);
	}

	public static java.lang.Object callField(haxe.lang.IHxObject obj, java.lang.String field, Array<?> args)
	{
		return obj.__hx_invokeField(field, args);
	}
')
@:keep class Runtime
{
	public static var undefined:Dynamic = { };

	@:functionCode('
	return new haxe.lang.Closure(obj, field);
	')
	public static function closure(obj:Dynamic, field:String):Dynamic
	{
		return null;
	}

	@:functionCode('
			if (v1 == v2)
				return true;
			if (v1 == null || v2 == null)
				return false;

			if (v1 instanceof java.lang.Number)
			{
				if (!(v2 instanceof java.lang.Number))
					return false;

				java.lang.Number v1c = (java.lang.Number) v1;
				java.lang.Number v2c = (java.lang.Number) v2;
				if (v1 instanceof java.lang.Long || v2 instanceof java.lang.Long)
					return v1c.longValue() == v2c.longValue();
				return v1c.doubleValue() == v2c.doubleValue();
			} else if (v1 instanceof java.lang.String || v1 instanceof haxe.lang.IEquatable) { //TODO see what happens with Boolean cases
				return v1.equals(v2);
			}

			return false;
	')
	public static function eq(v1:Dynamic, v2:Dynamic):Bool
	{
		return false;
	}

	@:functionCode('
		if (v1 == v2)
			return true;

		if (v1 instanceof java.lang.String || v1 instanceof haxe.lang.IEquatable)
		{
			return v1 != null && v1.equals(v2);
		} else {
			return v1 == v2;
		}
	')
	public static function refEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}

	@:functionCode('
		return v1 == v2 || (v1 != null && v1.equals(v2));
	')
	public static function valEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}

	@:functionCode('
		return (obj == null) ? 0.0 : ((java.lang.Number) obj).doubleValue();
	')
	public static function toDouble(obj:Dynamic):Float
	{
		return 0.0;
	}

	@:functionCode('
		return (obj == null) ? false : ((java.lang.Boolean) obj).booleanValue();
	')
	public static function toBool(obj:Dynamic):Bool
	{
		return false;
	}

	@:functionCode('
		return (obj == null) ? 0 : ((java.lang.Number) obj).intValue();
	')
	public static function toInt(obj:Dynamic):Int
	{
		return 0;
	}

	public static function toLong(obj:Dynamic):haxe.Int64
	{
		return obj == null ? 0 : (obj : java.lang.Number).longValue();
	}

	@:functionCode('
		if (obj != null && obj instanceof java.lang.Number)
		{
			return true;
		} else {
			return false;
		}
	')
	public static function isDouble(obj:Dynamic):Bool
	{
		return false;
	}

	@:overload public static function isInt(obj:Dynamic):Bool
	{
		if (Std.is(obj, java.lang.Number)) {
			var n:java.lang.Number = obj;
			return n.doubleValue() == n.intValue();
		} else {
			return false;
		}
	}

	@:overload public static function isInt(num:java.lang.Number):Bool {
		return num != null && num.doubleValue() == num.intValue();
	}

	@:functionCode('
		java.lang.Class cl = null;
		if (o instanceof java.lang.Class)
		{
			if (o == java.lang.String.class)
				return field.equals("fromCharCode");

			cl = (java.lang.Class) o;
		} else if (o instanceof java.lang.String) {
			return haxe.lang.StringRefl.handleGetField( (java.lang.String) o, field, false) != null;
		} else {
			cl = o.getClass();
		}

		try
		{
			java.lang.reflect.Field f = cl.getField(field);
			return true;
		}
		catch(Throwable t)
		{
			java.lang.reflect.Method[] ms = cl.getMethods();
			for (int i = 0; i < ms.length; i++)
			{
				if (ms[i].getName().equals(field))
				{
					return true;
				}
			}
		}

		return false;
	')
	public static function slowHasField(o:Dynamic, field:String):Bool
	{
		return false;
	}

	@:functionCode('
			if (v1 == v2)
				return 0;
			if (v1 == null) return -1;
			if (v2 == null) return 1;

			if (v1 instanceof java.lang.Number || v2 instanceof java.lang.Number)
			{
				java.lang.Number v1c = (java.lang.Number) v1;
				java.lang.Number v2c = (java.lang.Number) v2;

				if (v1 instanceof java.lang.Long || v2 instanceof java.lang.Long)
				{
					long l1 = (v1 == null) ? 0L : v1c.longValue();
					long l2 = (v2 == null) ? 0L : v2c.longValue();
          return (l1 < l2) ? -1 : (l1 > l2) ? 1 : 0;
				} else {
					double d1 = (v1 == null) ? 0.0 : v1c.doubleValue();
					double d2 = (v2 == null) ? 0.0 : v2c.doubleValue();

          return (d1 < d2) ? -1 : (d1 > d2) ? 1 : 0;
				}
			}
			//if it\'s not a number it must be a String
			return ((java.lang.String) v1).compareTo((java.lang.String) v2);
	')
	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		return 0;
	}

	@:functionCode('
			if (v1 instanceof java.lang.String || v2 instanceof java.lang.String)
				return toString(v1) + toString(v2);

			if (v1 instanceof java.lang.Number || v2 instanceof java.lang.Number)
			{
				java.lang.Number v1c = (java.lang.Number) v1;
				java.lang.Number v2c = (java.lang.Number) v2;

				double d1 = (v1 == null) ? 0.0 : v1c.doubleValue();
				double d2 = (v2 == null) ? 0.0 : v2c.doubleValue();

				return d1 + d2;
			}

			throw new java.lang.IllegalArgumentException("Cannot dynamically add " + v1 + " and " + v2);
	')
	public static function plus(v1:Dynamic, v2:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('

	if (obj == null)
		if (throwErrors)
			throw new java.lang.NullPointerException("Cannot access field \'" + field + "\' of null.");
		else
			return null;

	java.lang.Class cl = null;
	try
	{
		if (obj instanceof java.lang.Class)
		{
			if (obj == java.lang.String.class && field.equals("fromCharCode"))
				return new haxe.lang.Closure(haxe.lang.StringExt.class, field);

			cl = (java.lang.Class) obj;
			obj = null;
		} else if (obj instanceof java.lang.String) {
			return haxe.lang.StringRefl.handleGetField((java.lang.String) obj, field, throwErrors);
		} else {
			cl = obj.getClass();
		}

		java.lang.reflect.Field f = cl.getField(field);
		f.setAccessible(true);
		return f.get(obj);
	} catch (Throwable t)
	{
		try
		{
			java.lang.reflect.Method[] ms = cl.getMethods();
			for (int i = 0; i < ms.length; i++)
			{
				if (ms[i].getName().equals(field))
				{
					return new haxe.lang.Closure(obj != null ? obj : cl, field);
				}
			}
		} catch (Throwable t2)
		{

		}

		if (throwErrors)
			throw HaxeException.wrap(t);

		return null;
	}

	')
	public static function slowGetField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}

	@:functionCode('
		java.lang.Class cl = null;
		if (obj instanceof java.lang.Class)
		{
			cl = (java.lang.Class) obj;
			obj = null;
		} else {
			cl = obj.getClass();
		}

		try {
			java.lang.reflect.Field f = cl.getField(field);
			f.setAccessible(true);

			//FIXME we must evaluate if field to be set receives either int or double
			if (isInt(value))
			{
				f.setInt(obj, toInt(value));
			} else if (isDouble(value)) {
				f.setDouble(obj, toDouble(value));
			} else {
				f.set(obj, value);
			}
			return value;
		}
		catch (Throwable t)
		{
			throw HaxeException.wrap(t);
		}
	')
	public static function slowSetField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('
		java.lang.Class cl = null;
		if (obj instanceof java.lang.Class)
		{
			if (obj == java.lang.String.class && field.equals("fromCharCode"))
				return haxe.lang.StringExt.fromCharCode(toInt(args.__get(0)));

			cl = (java.lang.Class) obj;
			obj = null;
		} else if (obj instanceof java.lang.String) {
			return haxe.lang.StringRefl.handleCallField((java.lang.String) obj, field, args);
		} else {
			cl = obj.getClass();
		}

		if (args == null) args = new Array();

		int len = args.length;
		java.lang.Class[] cls = new java.lang.Class[len];
		java.lang.Object[] objs = new java.lang.Object[len];

		java.lang.reflect.Method[] ms = cl.getMethods();
		int msl = ms.length;
		int realMsl = 0;
		for(int i =0; i < msl; i++)
		{
			if (!ms[i].getName().equals(field) || (!ms[i].isVarArgs() && ms[i].getParameterTypes().length != len))
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
			if (o == null)
			{
				continue; //can be anything
			}
			objs[i]= o;
			cls[i] = o.getClass();
			boolean isNum = false;

			if (o instanceof java.lang.Number)
			{
				cls[i] = java.lang.Number.class;
				isNum = hasNumber = true;
			} else if (o instanceof java.lang.Boolean) {
				cls[i] = java.lang.Boolean.class;
				isNum = true;
			}

			msl = realMsl;
			realMsl = 0;

			for (int j = 0; j < msl; j++)
			{
				java.lang.Class[] allcls = ms[j].getParameterTypes();
				if (i < allcls.length)
				{
					if (!  ((isNum && allcls[i].isPrimitive()) || allcls[i].isAssignableFrom(cls[i])) )
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

		java.lang.reflect.Method found;
		if (ms.length == 0 || (found = ms[0]) == null)
			throw haxe.lang.HaxeException.wrap("No compatible method found for: " + field);

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
							} else if (name.equals("long") || name.equals("java.lang.Long"))
							{
								objs[i] = ((java.lang.Number)o).longValue();
							}
						}
					} //else varargs not handled TODO
				}
			}
		}

		try {
			found.setAccessible(true);
			return found.invoke(obj, objs);
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
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}

	@:functionCode('
		if (obj instanceof haxe.lang.IHxObject)
		{
			return ((haxe.lang.IHxObject) obj).__hx_invokeField(field, args);
		}

		return slowCallField(obj, field, args);
	')
	public static function callField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}

	@:functionCode('

		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_getField(field, throwErrors, false, false);

		return slowGetField(obj, field, throwErrors);

	')
	public static function getField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}

	@:functionCode('

		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_getField_f(field, throwErrors, false);

		return toDouble(slowGetField(obj, field, throwErrors));

	')
	public static function getField_f(obj:Dynamic, field:String, throwErrors:Bool):Float
	{
		return 0.0;
	}

	@:functionCode('

		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_setField(field, value, false);

		return slowSetField(obj, field, value);

	')
	public static function setField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('

		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_setField_f(field, value, false);

		return toDouble(slowSetField(obj, field, value));

	')
	public static function setField_f(obj:Dynamic, field:String, value:Float):Float
	{
		return 0.0;
	}

	public static function toString(obj:Dynamic):String
	{
		if (obj == null)
			return null;

		if (Std.is(obj, java.lang.Number) && !Std.is(obj, java.lang.Integer.IntegerClass) && isInt( (obj : java.lang.Number) ))
			return java.lang.Integer._toString(toInt(obj));
		return untyped obj.toString();
	}

	public static function isFinite(v:Float):Bool
	{
		return (v == v) && !java.lang.Double.DoubleClass._isInfinite(v);
	}
}

@:keep @:native("haxe.lang.EmptyObject") enum EmptyObject
{
	EMPTY;
}
