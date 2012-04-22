package haxe.lang;

/**
 * ...
 * @author waneck
 */

@:nativegen
//it's private so we don't have access to it in normal haxe code
@:native('haxe.lang.Runtime')
@:classContents('
	public static java.lang.Object getField(haxe.lang.IHxObject obj, java.lang.String field, boolean throwErrors)
	{
		if (obj == null && !throwErrors) return null;
		return obj.__hx_getField(field, false, throwErrors, false);
	}
	
	public static double getField_f(haxe.lang.IHxObject obj, java.lang.String field, boolean throwErrors)
	{
		if (obj == null && !throwErrors) return 0.0;
		return obj.__hx_getField_f(field, false, throwErrors);
	}
	
	public static java.lang.Object setField(haxe.lang.IHxObject obj, java.lang.String field, java.lang.Object value)
	{
		return obj.__hx_setField(field, false, value);
	}
	
	public static double setField_f(haxe.lang.IHxObject obj, java.lang.String field, double value)
	{
		return obj.__hx_setField_f(field, false, value);
	}
	
	public static java.lang.Object callField(haxe.lang.IHxObject obj, java.lang.String field, Array<?> args)
	{
		return obj.__hx_invokeField(field, false, args);
	}
')
@:keep private class Runtime 
{
	public static var undefined:Dynamic = {};
	
	@:functionBody('
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
	
	@:functionBody('
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
	
	@:functionBody('
		return v1 == v2 || (v1 != null && v1.equals(v2));
	')
	public static function valEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}
	
	@:functionBody('
		return (obj == null) ? 0.0 : ((java.lang.Number) obj).doubleValue();
	')
	public static function toDouble(obj:Dynamic):Float
	{
		return 0.0;
	}
	
	@:functionBody('
		return (obj == null) ? 0 : ((java.lang.Number) obj).intValue();
	')
	public static function toInt(obj:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
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
	
	@:functionBody('
		if (obj != null && obj instanceof java.lang.Number)
		{
			java.lang.Number n = (java.lang.Number) obj;
			return n.doubleValue() == n.intValue();
		} else {
			return false;
		}
	')
	public static function isInt(obj:Dynamic):Bool
	{
		return false;
	}
	
	@:functionBody('
		java.lang.Class cl = null;
		if (o instanceof java.lang.Class)
		{
			cl = (java.lang.Class) o;
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
	
	@:functionBody('
			if (v1 == v2)
				return 0;
			
			if (v1 instanceof java.lang.Number)
			{
				java.lang.Number v1c = (java.lang.Number) v1;
				java.lang.Number v2c = (java.lang.Number) v2;
				
				if (v1 instanceof java.lang.Long || v2 instanceof java.lang.Long)
				{
					long l1 = (v1 == null) ? 0L : v1c.longValue();
					long l2 = (v2 == null) ? 0L : v2c.longValue();
					return (int) (l1 - l2);
				} else {
					double d1 = (v1 == null) ? 0.0 : v1c.doubleValue();
					double d2 = (v2 == null) ? 0.0 : v2c.doubleValue();
					
					return (int) (d1 - d2);
				}
			}
			//if it\'s not a number it must be a String
			return ((java.lang.String) v1).compareTo((java.lang.String) v2);
	')
	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
			if (v1 instanceof java.lang.String || v2 instanceof java.lang.String)
				return (v1 + "") + (v2 + "");
			
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
	
	@:functionBody('
	
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
			cl = (java.lang.Class) obj;
			obj = null;
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
					return new haxe.lang.NativeMethodFunction(obj, field);
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
	
	@:functionBody('
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
	
	@:functionBody('
		java.lang.Class cl = null;
		if (obj instanceof java.lang.Class)
		{
			cl = (java.lang.Class) obj;
			obj = null;
		} else {
			cl = obj.getClass();
		}
		
		if (args == null) args = new Array();
		
		try {
			int len = args.length;
			java.lang.Class[] cls = new java.lang.Class[len];
			java.lang.Object[] objs = new java.lang.Object[len];
			
			java.lang.reflect.Method[] ms = cl.getDeclaredMethods();
			int msl = ms.length;
			int lstRes = 0;
			int realMsl = 0;
			for(int i =0; i < msl; i++)
			{
				if (ms[i].getName() != field || (!ms[i].isVarArgs() && ms[i].getParameterTypes().length != len))
				{
					ms[i] = null;
				} else {
					ms[lstRes] = ms[i];
					if (lstRes != i)
						ms[i] = null;
					lstRes = i + 1;
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
								lstRes = j + 1;
								realMsl++;
							}
						}
					}
					
					if (realMsl == 0)
						throw haxe.lang.HaxeException.wrap("No compatible method found for: " + field);
				} else {
					hasNumber = true;
				}
				
			}
			
			java.lang.reflect.Method found = ms[0];
			
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
			return found.invoke(obj, objs);
		} catch(Throwable t) {
			throw HaxeException.wrap(t);
		}
	')
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}
	
	@:functionBody('
		if (obj instanceof haxe.lang.IHxObject)
		{
			return ((haxe.lang.IHxObject) obj).__hx_invokeField(field, false, args);
		}
		
		return slowCallField(obj, field, args);
	')
	public static function callField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_getField(field, false, throwErrors, false);
		
		return slowGetField(obj, field, throwErrors);
	
	')
	public static function getField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_getField_f(field, false, throwErrors);
		
		return toDouble(slowGetField(obj, field, throwErrors));
	
	')
	public static function getField_f(obj:Dynamic, field:String, throwErrors:Bool):Float
	{
		return 0.0;
	}
	
	@:functionBody('
	
		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_setField(field, false, value);
		
		return slowSetField(obj, field, value);
	
	')
	public static function setField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		if (obj instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) obj).__hx_setField_f(field, false, value);
		
		return toDouble(slowSetField(obj, field, value));
	
	')
	public static function setField_f(obj:Dynamic, field:String, value:Float):Float
	{
		return 0.0;
	}
	
	
	private static var classes:Hash<Class<Dynamic>> = new Hash();
	
	public static function registerClass(name:String, cl:Class<Dynamic>):Void
	{
		classes.set(name, cl);
	}
	
	public static function getClass(name:String, t:java.lang.Class<Dynamic>):Class<Dynamic>
	{
		var ret:Class<Dynamic> = classes.get(name);
		if (ret == null)
			return slowGetClass(name, t);
		else
			return ret;
	}
	
	@:functionBody('
	if (t == null)
		return null;
	
	return null;
	')
	public static function slowGetClass(name:String, t:java.lang.Class<Dynamic>):Class<Dynamic>
	{
		return null;
	}
	
}

@:native("haxe.lang.EmptyObject") private enum EmptyObject
{
	EMPTY;
}