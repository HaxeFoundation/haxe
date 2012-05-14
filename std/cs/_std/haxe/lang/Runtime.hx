package haxe.lang;

/**
 * ...
 * @author waneck
 */

@:nativegen
//it's private so we don't have access to it in normal haxe code
@:native('haxe.lang.Runtime')
@:classContents('
	public static object getField(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return null;
		return obj.__hx_getField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, throwErrors, false);
	}
	
	public static double getField_f(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return 0.0;
		return obj.__hx_getField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, throwErrors);
	}
	
	public static object setField(haxe.lang.HxObject obj, string field, int fieldHash, object value)
	{
		return obj.__hx_setField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, value);
	}
	
	public static double setField_f(haxe.lang.HxObject obj, string field, int fieldHash, double value)
	{
		return obj.__hx_setField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, value);
	}
	
	public static object callField(haxe.lang.HxObject obj, string field, int fieldHash, Array args)
	{
		return obj.__hx_invokeField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, args);
	}
')
@:keep private class Runtime 
{
	public static var undefined:Dynamic = { };
	
	@:functionBody('
		if (obj is haxe.lang.IHxObject)
		{
			return new haxe.lang.Closure(field, hash, (haxe.lang.IHxObject)obj);
		} else {
			return new haxe.lang.NativeMethodFunction(obj, field);
		}
	')
	public static function closure(obj:Dynamic, hash:Int, field:String):Dynamic
	{
		return null;
	}
	
	@:functionBody('
			if (System.Object.ReferenceEquals(v1, v2))
				return true;
			if (v1 == null || v2 == null)
				return false;
			
			System.IConvertible v1c = v1 as System.IConvertible;
			
			if (v1c != null)
			{
				System.IConvertible v2c = v2 as System.IConvertible;
				
				if (v2c == null)
				{
					return false;
				}
				
				System.TypeCode t1 = v1c.GetTypeCode();
				System.TypeCode t2 = v2c.GetTypeCode();
				if (t1 == t2)
					return v1c.Equals(v2c);
				
				switch(t1)
				{
					case System.TypeCode.Int64:
					case System.TypeCode.UInt64:
						return v1c.ToUInt64(null) == v2c.ToUInt64(null);
					default:
						return v1c.ToDouble(null) == v2c.ToDouble(null);
				}
			}
			
			//add here haxe.lang.Equatable test
			
			return false;
	')
	public static function eq(v1:Dynamic, v2:Dynamic):Bool
	{
		return false;
	}
	
	@:functionBody('
			return System.Object.ReferenceEquals(v1, v2);
	')
	public static function refEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}
	
	@:functionBody('
			return (obj == null) ? 0.0 : ((System.IConvertible) obj).ToDouble(null);
	')
	public static function toDouble(obj:Dynamic):Float
	{
		return 0.0;
	}
	
	@:functionBody('
			return (obj == null) ? 0 : ((System.IConvertible) obj).ToInt32(null);
	')
	public static function toInt(obj:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
			System.IConvertible cv1 = v1 as System.IConvertible;
			if (cv1 != null)
			{
				System.IConvertible cv2 = v2 as System.IConvertible;
				
				if (cv2 == null)
				{
					throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
				}
				
				switch(cv1.GetTypeCode())
				{
					case System.TypeCode.String:
						if (cv2.GetTypeCode() != System.TypeCode.String)
							throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
						return v1.ToString().CompareTo(v2);
					/*case System.TypeCode.Int64:
					case System.TypeCode.UInt64:
						return ((int) (cv1.ToUInt64() - cv2.ToUInt64())) no Int64 operator support */
					default:
						return ((int) (cv1.ToDouble(null) - cv2.ToDouble(null)));
				}
			}
			
			System.IComparable c1 = v1 as System.IComparable;
			System.IComparable c2 = v2 as System.IComparable;
			
			if (c1 == null || c2 == null)
			{
				if (c1 == c2)
					return 0;
				
				throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
			}
			
			return c1.CompareTo(c2);
	')
	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
			if (v1 is string || v2 is string)
				return (v1 + "") + (v2 + "");
			
			System.IConvertible cv1 = v1 as System.IConvertible;
			if (cv1 != null)
			{
				System.IConvertible cv2 = v2 as System.IConvertible;
				
				if (cv2 == null)
				{
					throw new System.ArgumentException("Cannot dynamically add " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
				}
				
				return cv1.ToDouble(null) + cv2.ToDouble(null);
			}
			
			throw new System.ArgumentException("Cannot dynamically add " + v1 + " and " + v2);
	')
	public static function plus(v1:Dynamic, v2:Dynamic):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		if (obj == null)
			if (throwErrors) 
				throw new System.NullReferenceException("Cannot access field \'" + field + "\' of null.");
			else
				return null;
		
		System.Type t = obj as System.Type;
        if (t == null)
		{
			t = obj.GetType();
		} else {
			obj = null;
		}

		System.Reflection.FieldInfo f = t.GetField(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
		if (f != null)
		{
			return f.GetValue(obj);
		} else {
			System.Reflection.PropertyInfo prop = t.GetProperty(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
			if (prop == null)
			{
				System.Reflection.MethodInfo m = t.GetMethod(field,  System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
				if (m != null)
				{
					return new haxe.lang.NativeMethodFunction(obj, field);
				} else {
					if (throwErrors) 
						throw HaxeException.wrap("Cannot access field \'" + field + "\'.");
					else
						return null;
				}
			}
			return prop.GetValue(obj, null);
		}
	
	')
	public static function slowGetField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}
	
	@:functionBody('
		if (obj == null) return false;
		System.Type t = obj as System.Type;
        if (t == null)
		{
			t = obj.GetType();
		} else {
			obj = null;
		}

		System.Reflection.MemberInfo[] mi = t.GetMember(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
		return mi != null && mi.Length > 0;
	')
	public static function slowHasField(obj:Dynamic, field:String):Bool
	{
		return false;
	}
	
	@:functionBody('
		if (obj == null)
			throw new System.NullReferenceException("Cannot access field \'" + field + "\' of null.");
		
		System.Type t = obj as System.Type;
        if (t == null)
		{
			t = obj.GetType();
		} else {
			obj = null;
		}

		System.Reflection.FieldInfo f = t.GetField(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
		if (f != null)
		{
			f.SetValue(obj, @value);
			return @value;
		} else {
			System.Reflection.PropertyInfo prop = t.GetProperty(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
			prop.SetValue(obj, @value, null);

			return @value;
		}
		
	')
	public static function slowSetField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		//not implemented yet;
		throw "Not implemented";
	}
	
	@:functionBody('
		if (args == null) args = new Array<object>();
		
		System.Type t = obj as System.Type;
		if (t == null)
		{
			t = obj.GetType();
		}
		else
		{
			obj = null;
		}

		int length = (int)haxe.lang.Runtime.getField_f(args, "length", 520590566, true);
		object[] oargs = new object[length];
		System.Type[] ts = new System.Type[length];
		for (int i = 0; i < length; i++)
		{
			oargs[i] = args[i];
			ts[i] = oargs[i].GetType();
		}

		System.Reflection.MethodInfo mi = t.GetMethod(field, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.FlattenHierarchy | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance, null, ts, null);
		return mi.Invoke(obj, oargs);
	')
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		throw "not implemented";
	}
	
	@:functionBody('
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_invokeField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, args);
		
		return slowCallField(obj, field, args);
	')
	public static function callField(obj:Dynamic, field:String, fieldHash:Int, args:Array<Dynamic>):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_getField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, throwErrors, false);
		
		return slowGetField(obj, field, throwErrors);
	
	')
	public static function getField(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_getField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, throwErrors);
		
		return (double)slowGetField(obj, field, throwErrors);
	
	')
	public static function getField_f(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Float
	{
		return 0.0;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_setField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, value);
		
		return slowSetField(obj, field, value);
	
	')
	public static function setField(obj:Dynamic, field:String, fieldHash:Int, value:Dynamic):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_setField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, false, value);
		
		return (double)slowSetField(obj, field, value);
	
	')
	public static function setField_f(obj:Dynamic, field:String, fieldHash:Int, value:Float):Float
	{
		return 0.0;
	}
	
	
	private static var classes:Hash<Class<Dynamic>> = new Hash();
	
	public static function registerClass(name:String, cl:Class<Dynamic>):Void
	{
		classes.set(name, cl);
	}
	
	public static function getClass(name:String, t:system.Type):Class<Dynamic>
	{
		var ret:Class<Dynamic> = classes.get(name);
		if (ret == null)
			return slowGetClass(name, t);
		else
			return ret;
	}
	
	@:functionBody('
	if (t == null)
		t = System.Type.GetType(name, false, true);
	
	if (t == null)
		return null;
	
	return null;
	')
	public static function slowGetClass(name:String, t:system.Type):Class<Dynamic>
	{
		return null;
	}
	
}

@:native("haxe.lang.EmptyObject") private enum EmptyObject
{
	EMPTY;
}