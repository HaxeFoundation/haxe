/*
 * Copyright (C)2005-2019 Haxe Foundation
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
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi class Type {
	public static function getClass<T>(o:T):Class<T> {
		if (o == null || Std.isOfType(o, DynamicObject) || Std.isOfType(o, java.lang.Class)) {
			return null;
		}
		return cast java.Lib.getNativeType(o);
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		if (Std.isOfType(o, java.lang.Enum) || Std.isOfType(o, HxEnum)) {
			return untyped o.getClass();
		}
		return null;
	}

	public static function getSuperClass(c:Class<Dynamic>):Class<Dynamic> {
		var c = java.Lib.toNativeType(c);
		var cl:java.lang.Class<Dynamic> = c == null ? null : untyped c.getSuperclass();
		if (cl != null && cl.getName() != "haxe.lang.HxObject" && cl.getName() != "java.lang.Object") {
			return cast cl;
		}
		return null;
	}

	public static function getClassName(c:Class<Dynamic>):String {
		var c:java.lang.Class<Dynamic> = cast c;
		var name:String = c.getName();
		if (name.startsWith("haxe.root."))
			return name.substr(10);
		if (name.startsWith("java.lang"))
			name = name.substr(10);

		return switch (name) {
			case "int", "Integer": "Int";
			case "double", "Double": "Float";
			case "Object": "Dynamic";
			default: name;
		}
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		var c:java.lang.Class<Dynamic> = cast e;
		var ret:String = c.getName();
		if (ret.startsWith("haxe.root."))
			return ret.substr(10);

		return ret;
	}

	public static function resolveClass(name:String):Class<Dynamic> {
		try {
			if (name.indexOf(".") == -1) {
				name = "haxe.root." + name;
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
	public static function resolveEnum(name:String):Enum<Dynamic>
		untyped {
			if (name == "Bool")
				return Bool;
			return resolveClass(name);
		}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		var nargs = args.length,
			callArguments = new java.NativeArray<Dynamic>(nargs);

		var ctors = java.Lib.toNativeType(cl).getConstructors(),
			totalCtors = ctors.length,
			validCtors = 0;

		for (i in 0...totalCtors) {
			var ctor = ctors[i];
			var ptypes = ctor.getParameterTypes();
			if (ptypes.length != nargs && !ctor.isVarArgs()) {
				continue;
			}

			var argNum = -1, valid = true;
			for (arg in args) {
				argNum++;
				var expectedType = argNum < ptypes.length ? ptypes[argNum] : ptypes[ptypes.length - 1]; // varags
				var isDynamic = Std.isOfType(arg, DynamicObject) && expectedType.isAssignableFrom(java.Lib.getNativeType(arg));
				var argType = Type.getClass(arg);

				if (arg == null || isDynamic || (argType != null && expectedType.isAssignableFrom(java.Lib.toNativeType(argType)))) {
					callArguments[argNum] = arg;
				} else if(expectedType.getName() == 'boolean' && (cast argType:java.lang.Class<Dynamic>).getName() == 'java.lang.Boolean') {
					callArguments[argNum] = (cast arg : java.lang.Boolean).booleanValue();
				} else if (Std.isOfType(arg, java.lang.Number)) {
					var name = expectedType.getName();
					switch (name) {
						case 'double' | 'java.lang.Double':
							callArguments[argNum] = (cast arg : java.lang.Number).doubleValue();
						case 'int' | 'java.lang.Integer':
							callArguments[argNum] = (cast arg : java.lang.Number).intValue();
						case 'float' | 'java.lang.Float':
							callArguments[argNum] = (cast arg : java.lang.Number).floatValue();
						case 'byte' | 'java.lang.Byte':
							callArguments[argNum] = (cast arg : java.lang.Number).byteValue();
						case 'short' | 'java.lang.Short':
							callArguments[argNum] = (cast arg : java.lang.Number).shortValue();
						case _:
							valid = false;
							break;
					}
				} else {
					valid = false;
					break;
				}
			}
			if (!valid) {
				continue;
			}

			// the current constructor was found and it is valid - call it
			ctor.setAccessible(true);
			return cast ctor.newInstance(callArguments);
		}

		throw 'Could not find any constructor that matches the provided arguments for class $cl';
	}

	// cache empty constructor arguments so we don't allocate it on each createEmptyInstance call
	@:protected @:readOnly static var __createEmptyInstance_EMPTY_TYPES = java.Lib.toNativeEnum(java.internal.Runtime.EmptyObject);
	@:protected @:readOnly static var __createEmptyInstance_EMPTY_ARGS = java.internal.Runtime.EmptyObject.EMPTY;

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		var t = java.Lib.toNativeType(cl);
		try {
			var ctor = t.getConstructor(__createEmptyInstance_EMPTY_TYPES);
			return ctor.newInstance(__createEmptyInstance_EMPTY_ARGS);
		} catch (_:java.lang.NoSuchMethodException) {
			return t.newInstance();
		}
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		if (params == null || params.length == 0) {
			var ret:Dynamic = java.internal.Runtime.slowGetField(e, constr, true);
			if (Std.isOfType(ret, java.internal.Function)) {
				throw "Constructor " + constr + " needs parameters";
			}
			return ret;
		} else {
			var params = java.Lib.nativeArray(params, true);
			return java.internal.Runtime.slowCallField(e, constr, params);
		}
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
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
	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
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
	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return null;
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		if (Reflect.hasField(e, "__hx_constructs")) {
			var ret:Array<String> = java.Lib.array(untyped e.__hx_constructs);
			return ret.copy();
		}
		var vals:java.NativeArray<java.lang.Enum<Dynamic>> = untyped e.values(),
			ret = [];
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
	public static function typeof(v:Dynamic):ValueType
		untyped {
			return null;
		}

	@:functionCode('
			if (a instanceof haxe.lang.Enum)
				return a.equals(b);
			else
				return haxe.lang.Runtime.eq(a, b);
	')
	public static function enumEq<T>(a:T, b:T):Bool
		untyped {
			return a == b;
		}

	@:functionCode('
		if (e instanceof java.lang.Enum)
			return ((java.lang.Enum) e).name();
		else
			return ((haxe.lang.Enum) e).getTag();
	')
	public static function enumConstructor(e:EnumValue):String
		untyped {
			return null;
		}

	@:functionCode('
		return ( e instanceof java.lang.Enum ) ? new haxe.root.Array() : ((haxe.lang.Enum) e).getParams();
	')
	public static function enumParameters(e:EnumValue):Array<Dynamic>
		untyped {
			return null;
		}

	@:ifFeature("has_enum")
	@:functionCode('
		if (e instanceof java.lang.Enum)
			return ((java.lang.Enum) e).ordinal();
		else
			return ((haxe.lang.Enum) e).index;
	')
	public static function enumIndex(e:EnumValue):Int
		untyped {
			return e.index;
		}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		var ctors = getEnumConstructs(e);
		var ret = [];
		for (ctor in ctors) {
			var v = Reflect.field(e, ctor);
			if (Std.isOfType(v, e))
				ret.push(v);
		}

		return ret;
	}
}
