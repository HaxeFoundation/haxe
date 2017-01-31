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
package cs.internal;
import cs.Lib;
import cs.Lib.*;
import cs.NativeArray;
import cs.StdTypes;
import cs.system.Activator;
import cs.system.IConvertible;
import cs.system.IComparable;
import cs.system.reflection.MethodBase;
import cs.system.reflection.MethodInfo;
import cs.system.reflection.*;
import cs.system.Type;
import cs.system.Object;

/**
 This class is meant for internal compiler use only. It provides the Haxe runtime
 compatibility to the host language.
**/

@:nativeGen
@:native('haxe.lang.Runtime')
@:access(String)
@:classCode('
	public static object getField(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return null;
		return obj.__hx_getField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false, false);
	}

	public static double getField_f(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return 0.0;
		return obj.__hx_getField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false);
	}

	public static object setField(haxe.lang.HxObject obj, string field, int fieldHash, object value)
	{
		return obj.__hx_setField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
	}

	public static double setField_f(haxe.lang.HxObject obj, string field, int fieldHash, double value)
	{
		return obj.__hx_setField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
	}

	public static object callField(haxe.lang.HxObject obj, string field, int fieldHash, Array args)
	{
		return obj.__hx_invokeField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, args);
	}
')
@:keep class Runtime
{
	@:readOnly public static var undefined(default, never):Dynamic = new cs.system.Object();

	public static function closure(obj:Dynamic, hash:Int, field:String):Dynamic
	{
		return new cs.internal.Function.Closure(obj, field, hash);
	}

	public static function eq(v1:Dynamic, v2:Dynamic):Bool
	{
		if (Object.ReferenceEquals(v1, v2))
			return true;
		if (Object.ReferenceEquals(v1,null) || Object.ReferenceEquals(v2,null))
			return false;

		var v1c = Lib.as(v1, IConvertible);
		if (v1c != null)
		{
			var v2c = Lib.as(v2, IConvertible);
			if (v2c == null)
			{
				return false;
			}

			var t1 = v1c.GetTypeCode(),
					t2 = v2c.GetTypeCode();
			if (t1 == t2)
				return Object._Equals(v1c,v2c);

			if (t1 == cs.system.TypeCode.String || t2 == cs.system.TypeCode.String)
				return false;

			switch [t1,t2]
			{
				case [Decimal, _] | [_, Decimal]:
					return v1c.ToDecimal(null) == v2c.ToDecimal(null);
				case [Int64, _] | [_, Int64]:
					return v1c.ToInt64(null) == v2c.ToInt64(null);
				case [UInt64 | DateTime, _] | [_, UInt64 | DateTime]:
					return v1c.ToUInt64(null) == v2c.ToUInt64(null);
				case [Double | Single, _] | [_, Double | Single]:
					return v1c.ToDouble(null) == v2c.ToDouble(null);
				case _:
					return v1c.ToInt32(null) == v2c.ToInt32(null);
			}
		}

		var v1v = Lib.as(v1, cs.system.ValueType);
		if (v1v != null)
		{
			return v1.Equals(v2);
#if !erase_generics
		} else {
			var v1t = Lib.as(v1, Type);
			if (v1t != null)
			{
				var v2t = Lib.as(v2, Type);
				if (v2t != null)
					return typeEq(v1t, v2t);
				return false;
			}
#end
		}

		return false;
	}

	public static function refEq(v1: { }, v2: { } ):Bool
	{
#if !erase_generics
		if (Std.is(v1, Type))
			return typeEq(Lib.as(v1,Type), Lib.as(v2,Type));
#end
		return Object.ReferenceEquals(v1,v2);
	}

	public static function toDouble(obj:Dynamic):Float
	{
		return (obj == null) ? .0 : Std.is(obj,Float) ? cast obj : Lib.as(obj,IConvertible).ToDouble(null);
	}

	public static function toInt(obj:Dynamic):Int
	{
		return (obj == null) ? 0 : Std.is(obj,Int) ? cast obj : Lib.as(obj,IConvertible).ToInt32(null);
	}

#if erase_generics
	public static function toLong(obj:Dynamic):Int64
	{
		return (obj == null) ? 0 : Std.is(obj,Int64) ? cast obj : Lib.as(obj,IConvertible).ToInt64(null);
	}
#end

	public static function isInt(obj:Dynamic):Bool
	{
		var cv1 = Lib.as(obj, IConvertible);
		if (cv1 != null)
		{
			switch (cv1.GetTypeCode())
			{
				case Double:
					var d:Float = cast obj;
					return d >= cs.system.Int32.MinValue && d <= cs.system.Int32.MaxValue && d == ( cast(d,Int) );
				case UInt32, Int32:
					return true;
				default:
					return false;
			}
		}
		return false;
	}

	public static function isUInt(obj:Dynamic):Bool
	{
		var cv1 = Lib.as(obj, IConvertible);
		if (cv1 != null)
		{
			switch (cv1.GetTypeCode())
			{
				case Double:
					var d:Float = cast obj;
					return d >= cs.system.UInt32.MinValue && d <= cs.system.UInt32.MaxValue && d == ( cast(d,UInt) );
				case UInt32:
					return true;
				default:
					return false;
			}

		}
		return false;
	}

	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		if (Object.ReferenceEquals(v1,v2)) return 0;
		if (Object.ReferenceEquals(v1,null)) return -1;
		if (Object.ReferenceEquals(v2,null)) return 1;

		var cv1 = Lib.as(v1, IConvertible);
		if (cv1 != null)
		{
			var cv2 = Lib.as(v2, IConvertible);

			if (cv2 == null)
			{
				throw new cs.system.ArgumentException("Cannot compare " + getNativeType(v1).ToString() + " and " + getNativeType(v2).ToString());
			}

			switch(cv1.GetTypeCode())
			{
				case cs.system.TypeCode.String:
					if (cv2.GetTypeCode() != cs.system.TypeCode.String)
						throw new cs.system.ArgumentException("Cannot compare " + getNativeType(v1).ToString() + " and " + getNativeType(v2).ToString());
					var s1 = Lib.as(v1,String);
					var s2 = Lib.as(v2,String);
					return String.Compare(s1,s2, cs.system.StringComparison.Ordinal);
				case cs.system.TypeCode.Double:
					var d1:Float = cast v1,
							d2:Float = cv2.ToDouble(null);
					return (d1 < d2) ? -1 : (d1 > d2) ? 1 : 0;
				default:
					var d1d = cv1.ToDouble(null);
					var d2d = cv2.ToDouble(null);
					return (d1d < d2d) ? -1 : (d1d > d2d) ? 1 : 0;
			}
		}

		var c1 = Lib.as(v1, IComparable);
		var c2 = Lib.as(v2, IComparable);

		if (c1 == null || c2 == null)
		{
			throw new cs.system.ArgumentException("Cannot compare " + getNativeType(v1).ToString() + " and " + getNativeType(v2).ToString());
		}

		return c1.CompareTo(c2);
	}

	public static function plus(v1:Dynamic, v2:Dynamic):Dynamic
	{
		if (Std.is(v1,String) || Std.is(v2,String))
			return Std.string(v1) + Std.string(v2);

		if (v1 == null)
		{
			if (v2 == null) return null;
			v1 = 0;
		} else if (v2 == null) v2 = 0;

		var cv1 = Lib.as(v1, IConvertible);
		if (cv1 != null)
		{
			var cv2 = Lib.as(v2, IConvertible);
			if (cv2 == null)
			{
				throw new cs.system.ArgumentException("Cannot dynamically add " + cs.Lib.getNativeType(v1).ToString() + " and " + cs.Lib.getNativeType(v2).ToString());
			}
			return cv1.ToDouble(null) + cv2.ToDouble(null);
		}

		throw new cs.system.ArgumentException("Cannot dynamically add " + v1 + " and " + v2);
	}

	public static function slowGetField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		if (obj == null)
			if (throwErrors)
				throw new cs.system.NullReferenceException("Cannot access field \'" + field + "\' of null.");
			else
				return null;

		var t = Lib.as(obj, cs.system.Type);
		var bf =
			if (t == null)
			{
				var s = Lib.as(obj, String);
				if (s != null)
					return cs.internal.StringExt.StringRefl.handleGetField(s, field, throwErrors);
				t = obj.GetType();
				new cs.Flags(BindingFlags.Instance) | BindingFlags.Public | BindingFlags.FlattenHierarchy;
			} else {
				if (t == Lib.toNativeType(String) && field == "fromCharCode")
					return new cs.internal.Function.Closure(StringExt, field, 0);

				obj = null;
				new cs.Flags(BindingFlags.Static) | BindingFlags.Public;
			}

		var f = t.GetField(field, bf);
		if (f != null)
		{
			return unbox(f.GetValue(obj));
		}
		else
		{
			var prop = t.GetProperty(field, bf);
			if (prop == null)
			{
				var m = t.GetMember(field, bf);
				if (m.length == 0 && (field == "__get" || field == "__set"))
					m = t.GetMember(field == "__get" ? "get_Item" : "set_Item", bf);

				if (m.Length > 0)
				{
					return new cs.internal.Function.Closure(obj != null ? obj : t, field, 0);
				}
				else
				{
					// COM object handling
					if (t.IsCOMObject)
					{
						try
						{
							return t.InvokeMember(field, BindingFlags.GetProperty, null, obj, new cs.NativeArray(0));
						}
						catch (e:cs.system.Exception)
						{
							//Closures of COM objects not supported currently
						}
					}

					if (throwErrors)
						throw "Cannot access field \'" + field + "\'.";
					else
						return null;
				}
			}
			return unbox(prop.GetValue(obj, null));
		}
	}

	public static function slowHasField(obj:Dynamic, field:String):Bool
	{
		if (obj == null) return false;
		var t = Lib.as(obj, cs.system.Type);
		var bf =
			if (t == null) {
				var s = Lib.as(obj, String);
				if (s != null)
					return cs.internal.StringExt.StringRefl.handleGetField(s, field, false) != null;
				t = obj.GetType();
				new cs.Flags(BindingFlags.Instance) | BindingFlags.Public | BindingFlags.FlattenHierarchy;
			} else {
				if (t == Lib.toNativeType(String))
					return field == "fromCharCode";
				obj = null;
				new cs.Flags(BindingFlags.Static) | BindingFlags.Public;
			}
		var mi = t.GetMember(field, bf);
		return mi != null && mi.length > 0;
	}

	public static function slowSetField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		if (obj == null)
			throw new cs.system.NullReferenceException("Cannot access field \'" + field + "\' of null.");

		var t = Lib.as(obj, cs.system.Type);
		var bf =
			if (t == null)
			{
				t = obj.GetType();
				new cs.Flags(BindingFlags.Instance) | BindingFlags.Public | BindingFlags.FlattenHierarchy;
			} else {
				obj = null;
				new cs.Flags(BindingFlags.Static) | BindingFlags.Public;
			}

		var f = t.GetField(field, bf);
		if (f != null)
		{
			if (f.FieldType.ToString().StartsWith("haxe.lang.Null"))
			{
				value = mkNullable(value, f.FieldType);
			}
			if (value != null && Object.ReferenceEquals(Lib.toNativeType(cs.system.Double), Lib.getNativeType(value)) && !Object.ReferenceEquals(t, f.FieldType))
			{
				var ic = Lib.as(value, IConvertible);
				value = ic.ToType(f.FieldType, null);
			}

			f.SetValue(obj, value);
			return value;
		}
		else
		{
			var prop = t.GetProperty(field, bf);
			if (prop == null)
			{
				// COM object handling
				if (t.IsCOMObject)
				{
					try
					{
						return t.InvokeMember(field, BindingFlags.SetProperty, null, obj, cs.NativeArray.make(value));
					}
					catch (e:cs.system.Exception)
					{
						//Closures of COM objects not supported currently
					}
				}
				throw "Field \'" + field + "\' not found for writing from Class " + t;
			}

			if (prop.PropertyType.ToString().StartsWith("haxe.lang.Null"))
			{
				value = mkNullable(value, prop.PropertyType);
			}
			if (Object.ReferenceEquals(Lib.toNativeType(cs.system.Double), Lib.getNativeType(value)) && !Object.ReferenceEquals(t, f.FieldType))
			{
				var ic = Lib.as(value, IConvertible);
				value = ic.ToType(f.FieldType, null);
			}
			prop.SetValue(obj, value, null);

			return value;
		}
	}

	public static function callMethod(obj:Dynamic, methods:NativeArray<MethodBase>, methodLength:Int, args:Array<Dynamic>):Dynamic
	{
		if (methodLength == 0) throw "No available methods";
		var length = args.length;
		var oargs:NativeArray<Dynamic> = new NativeArray(length);
		var ts:NativeArray<Type> = new NativeArray(length);
		var rates:NativeArray<Int> = new NativeArray(methods.Length);

		for (i in 0...length)
		{
			oargs[i] = args[i];
			if (args[i] != null)
				ts[i] = Lib.getNativeType(args[i]);
		}

		var last = 0;

		//first filter by number of parameters and if it is assignable
		if (methodLength > 1)
		{
			for (i in 0...methodLength)
			{
				var params = methods[i].GetParameters();
				if (params.Length != length) {
					continue;
				} else {
					var fits = true, crate = 0;
					for (i in 0...params.Length)
					{
						var param = params[i].ParameterType;
						var strParam = param + "";
						if (param.IsAssignableFrom(ts[i]) || (ts[i] == null && !param.IsValueType))
						{
							//if it is directly assignable, we'll give it top rate
							continue;
						} else if (untyped strParam.StartsWith("haxe.lang.Null") || ( (oargs[i] == null || Std.is(oargs[i], IConvertible) ) && cast(untyped __typeof__(IConvertible), Type).IsAssignableFrom(param) ))
						{
							//if it needs conversion, give a penalty. TODO rate penalty
							crate++;
							continue;
						} else if (!param.ContainsGenericParameters) { //generics don't appear as assignable, but may be in the end. no rate there.
							fits = false;
							break;
						}
					}

					if (fits)
					{
						rates[last] = crate;
						methods[last++] = methods[i];
					}
				}
			}

			methodLength = last;
		} else if (methodLength == 1 && methods[0].GetParameters().Length != length) {
			methodLength = 0;
		}

		//At this time, we should be left with only one method.
		//Of course, realistically, we can be left with plenty of methods, if there are lots of variants with IConvertible
		//But at this time we still aren't rating the best methods
		//FIXME rate best methods

		if (methodLength == 0)
			throw "Invalid calling parameters for method " + methods[0].Name;

		var best = cs.system.Double.PositiveInfinity;
		var bestMethod = 0;
		for(i in 0...methodLength)
		{
			if (rates[i] < best)
			{
				bestMethod = i;
				best = rates[i];
			}
		}

		methods[0] = methods[bestMethod];
		var params = methods[0].GetParameters();
		for (i in 0...params.Length)
		{
			var param = params[i].ParameterType;
			var strParam = param + "",
					arg = oargs[i];
			if (StringTools.startsWith(strParam, "haxe.lang.Null"))
			{
				oargs[i] = mkNullable(arg, param);
			} else if (cast(untyped __typeof__(IConvertible), Type).IsAssignableFrom(param)) {
				if (arg == null) {
					if (param.IsValueType)
						oargs[i] = Activator.CreateInstance(param);
				} else if (!cs.Lib.getNativeType(arg).IsAssignableFrom(param)) {
					oargs[i] = cast(arg, IConvertible).ToType(param, null);
				}
			}
		}

		if (methods[0].ContainsGenericParameters && Std.is(methods[0], cs.system.reflection.MethodInfo))
		{
			var m:MethodInfo = cast methods[0];
			var tgs = m.GetGenericArguments();
			for (i in 0...tgs.Length)
			{
				tgs[i] = untyped __typeof__(Dynamic);
			}
			m = m.MakeGenericMethod(tgs);
			var retg = m.Invoke(obj, oargs);
			return cs.internal.Runtime.unbox(retg);
		}

		var m = methods[0];
		if (obj == null && Std.is(m, cs.system.reflection.ConstructorInfo))
		{
			var ret = cast(m, cs.system.reflection.ConstructorInfo).Invoke(oargs);
			return unbox(ret);
		}

		var ret = m.Invoke(obj, oargs);
		return unbox(ret);
	}

	public static function unbox(dyn:Dynamic):Dynamic
	{
		if (dyn != null && untyped (Lib.getNativeType(dyn) + "").StartsWith("haxe.lang.Null"))
		{
			return dyn.toDynamic();
		} else {
			return dyn;
		}
	}

#if !erase_generics
	@:functionCode('
		if (nullableType.ContainsGenericParameters)
			return haxe.lang.Null<object>.ofDynamic<object>(obj);
		return nullableType.GetMethod("_ofDynamic").Invoke(null, new object[] { obj });
	')
	public static function mkNullable(obj:Dynamic, nullableType:Type):Dynamic
	{
		return null;
	}
#else
	public static function mkNullable(obj:Dynamic, nullable:Type):Dynamic
	{
		return obj; //do nothing
	}
#end

	// @:functionCode('
	// 	if (field == "toString")
	// 	{
	// 		if (args == null)
	// 			return obj.ToString();
	// 		field = "ToString";
	// 	}
	// 	if (args == null) args = new Array<object>();

	// 	System.Reflection.BindingFlags bf;
	// 	System.Type t = obj as System.Type;
	// 	if (t == null)
	// 	{
	// 		string s = obj as string;
	// 		if (s != null)
	// 			return haxe.lang.StringRefl.handleCallField(s, field, args);
	// 		t = obj.GetType();
	// 		bf = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.FlattenHierarchy;
	// 	} else {
	// 		if (t == typeof(string) && field.Equals("fromCharCode"))
	// 			return haxe.lang.StringExt.fromCharCode(toInt(args[0]));
	// 		obj = null;
	// 		bf = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public;
	// 	}

	// 	System.Reflection.MethodInfo[] mis = t.GetMethods(bf);
	// 	int last = 0;
	// 	for (int i = 0; i < mis.Length; i++)
	// 	{
	// 		string name = mis[i].Name;
	// 		if (name.Equals(field))
	// 		{
	// 			mis[last++] = mis[i];
	// 		}
	// 	}

	// 	if (last == 0 && (field == "__get" || field == "__set"))
	// 	{
	// 		field = field == "__get" ? "get_Item" : "set_Item";
	// 		for (int i = 0; i < mis.Length; i++)
	// 		{
	// 			string name = mis[i].Name;
	// 			if (name.Equals(field))
	// 			{
	// 				mis[last++] = mis[i];
	// 			}
	// 		}
	// 	}

	// 	if (last == 0 && t.IsCOMObject)
	// 	{
	// 		object[] oargs = new object[arrLen(args)];
	// 		for (int i = 0; i < oargs.Length; i++)
	// 		{
	// 			oargs[i] = args[i];
	// 		}
	// 		return t.InvokeMember(field, System.Reflection.BindingFlags.InvokeMethod, null, obj, oargs);
	// 	}

	// 	if (last == 0)
	// 	{
	// 		throw haxe.lang.HaxeException.wrap("Method \'" + field + "\' not found on type " + t);
	// 	}

	// 	return haxe.lang.Runtime.callMethod(obj, mis, last, args);
	// ')
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		if (field == "toString" && (args == null || args.length == 0))
		{
			return obj.ToString();
		}
		if (args == null) args = [];

		var bf:BindingFlags;
		var t = Lib.as(obj,cs.system.Type);
		if (t == null)
		{
			var s = Lib.as(obj,String);
			if (s != null)
				return cs.internal.StringExt.StringRefl.handleCallField(untyped s, untyped field, args);
			t = untyped obj.GetType();
			bf = new Flags(BindingFlags.Instance) | BindingFlags.Public | BindingFlags.FlattenHierarchy;
		} else {
			if (t == Lib.toNativeType(String) && field == 'fromCharCode')
				return cs.internal.StringExt.fromCharCode(toInt(args[0]));
			obj = null;
			bf = new Flags(BindingFlags.Static) | BindingFlags.Public;
		}

		var mis:NativeArray<MethodBase> = untyped t.GetMethods(bf);
		var last = 0;
		for (i in 0...mis.Length)
		{
			var name = mis[i].Name;
			if (name == field)
				mis[last++] = mis[i];
		}

		if (last == 0 && (field == "__get" || field == "__set"))
		{
			field = field == "__get" ? "get_Item" : "set_Item";
			for (i in 0...mis.Length)
			{
				var name = mis[i].Name;
				if (name == field)
				{
					mis[last++] = mis[i];
				}
			}
		}

		if (last == 0 && t.IsCOMObject)
		{
			var oargs = new NativeArray(args.length);
			for (i in 0...oargs.Length)
			{
				oargs[i] = args[i];
			}
			return t.InvokeMember(field, BindingFlags.InvokeMethod, null, obj, oargs);
		}

		if (last == 0)
		{
			throw 'Method "$field" not found on type $t';
		}

		return Runtime.callMethod(obj, mis, last, args);
	}

	public static function callField(obj:Dynamic, field:String, fieldHash:Int, args:Array<Dynamic>):Dynamic
	{
		var hxObj = Lib.as(obj, HxObject);
		if (hxObj != null)
			return untyped hxObj.__hx_invokeField(field, (fieldHash == 0) ? FieldLookup.hash(field) : fieldHash, args);
		return slowCallField(obj, field, args);
	}

	public static function getField(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Dynamic
	{
		var hxObj = Lib.as(obj, HxObject);
		if (hxObj != null)
			return untyped hxObj.__hx_getField(field, (fieldHash == 0) ? FieldLookup.hash(field) : fieldHash, throwErrors, false, false);

		return slowGetField(obj, field, throwErrors);
	}

	public static function getField_f(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Float
	{
		var hxObj = Lib.as(obj, HxObject);
		if (hxObj != null)
			return untyped hxObj.__hx_getField_f(field, (fieldHash == 0) ? FieldLookup.hash(field) : fieldHash, throwErrors, false);

		return toDouble(slowGetField(obj, field, throwErrors));
	}

	public static function setField(obj:Dynamic, field:String, fieldHash:Int, value:Dynamic):Dynamic
	{
		var hxObj = Lib.as(obj, HxObject);
		if (hxObj != null)
			return untyped hxObj.__hx_setField(field, (fieldHash == 0) ? FieldLookup.hash(field) : fieldHash, value, false);

		return slowSetField(obj, field, value);
	}

	public static function setField_f(obj:Dynamic, field:String, fieldHash:Int, value:Float):Float
	{
		var hxObj = Lib.as(obj, HxObject);
		if (hxObj != null)
			return untyped hxObj.__hx_setField_f(field, (fieldHash == 0) ? FieldLookup.hash(field) : fieldHash, value, false);

		return toDouble(slowSetField(obj, field, value));
	}

	public static function toString(obj:Dynamic):String
	{
		if (obj == null)
			return null;
		if (Std.is(obj, Bool))
			if(obj)
				return "true";
			else
				return "false";

		return untyped obj.ToString();
	}

#if erase_generics
	inline
#end
	public static function typeEq(t1:Type, t2:Type):Bool
	{
		if (t1 == null || t2 == null)
			return t1 == t2;
#if !erase_generics
		var t1i = t1.IsInterface,
		    t2i = t2.IsInterface;
		if (t1i != t2i)
		{
			if (t1i)
			{
				var g = getGenericAttr(t1);
				if (g != null)
					t1 = g.generic;
			} else {
				var g = getGenericAttr(t2);
				if (g != null)
					t2 = g.generic;
			}
		}

#end
		if (t1.GetGenericArguments().Length > 0) t1 = t1.GetGenericTypeDefinition();
		if (t2.GetGenericArguments().Length > 0) t2 = t2.GetGenericTypeDefinition();
		return Object.ReferenceEquals(t1,t2);
	}


#if !erase_generics
	public static function getGenericAttr(t:cs.system.Type):cs.internal.HxObject.GenericInterface
	{
		for (attr in t.GetCustomAttributes(true))
			if (Std.is(attr,cs.internal.HxObject.GenericInterface))
				return cast attr;
		return null;
	}
#end

#if !erase_generics
	@:functionCode('
		if (obj is To)
			return (To) obj;
		else if (obj == null)
			return default(To);
		if (typeof(To) == typeof(double))
			return (To)(object) toDouble(obj);
		else if (typeof(To) == typeof(int))
			return (To)(object) toInt(obj);
		else if (typeof(To) == typeof(float))
			return (To)(object)(float)toDouble(obj);
		else if (typeof(To) == typeof(long))
			return (To)(object)(long)toDouble(obj);
		else
			return (To) obj;
	')
	public static function genericCast<To>(obj:Dynamic):To
	{
		return null;
	}
#end

	@:functionCode('
		return (s1 == null ? "null" : s1) + (s2 == null ? "null" : s2);
	')
	public static function concat(s1:String, s2:String):String
	{
		return null;
	}

	public static function toBool(dyn:Dynamic):Bool
	{
		return if (dyn == null) false else untyped __cs__("(bool){0}", dyn);
	}


	//TODO: change from genericCast to getConverter, so we don't need to handle extra boxing associated with it
	/*@:functionCode('
		if (typeof(To).TypeHandle == typeof(double).TypeHandle)
			return (System.Converter<object,To>) new System.Converter<object,double>(toDouble);
		else if (typeof(To).TypeHandle == typeof(double).TypeHandle)
			return (System.Converter<object,To>) new System.Converter<object,double>(toDouble);
		else
			return (System.Converter<object, To>) delegate(object obj) { return (To) obj; };
	')
	public static function getConverter<To>():cs.system.Converter<Dynamic,To>
	{
		return null;
	}*/
}

@:nativeGen
@:keep @:native("haxe.lang.EmptyObject") enum EmptyObject
{
	EMPTY;
}
