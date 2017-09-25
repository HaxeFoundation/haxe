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

import php.*;
import php.reflection.*;
import haxe.extern.EitherType;

using php.Global;

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

	public static function getClass<T>( o : T ) : Class<T> {
		if(Global.is_object(o) && !Boot.isClass(o) && !Boot.isEnumValue(o)) {
			var cls = Boot.getClass(Global.get_class(cast o));
			return (cls == Boot.getHxAnon() ? null : cast cls);
		} else if(Global.is_string(o)) {
			return cast String;
		} else {
			return null;
		}
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> {
		if(o == null) return null;
		return cast Boot.getClass(Global.get_class(cast o));
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		if(c == null) return null;
		var parentClass = Global.get_parent_class((cast c).phpClassName);
		if(!parentClass) return null;
		return cast Boot.getClass(parentClass);
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		if(c == null) return null;
		return Boot.getHaxeName(cast c);
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return getClassName(cast e);
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		if (name == null) return null;
		switch(name) {
			case 'Dynamic': return cast Dynamic;
			case 'Int': return cast Int;
			case 'Float': return cast Float;
			case 'Bool':  return cast Bool;
			case 'String': return String;
			case 'Class': return cast Class;
			case 'Enum': return cast Enum;
		}

		var phpClass = Boot.getPhpName(name);
		if (!Global.class_exists(phpClass) && !Global.interface_exists(phpClass)) return null;

		var hxClass = Boot.getClass(phpClass);

		return cast hxClass;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> {
		if (name == null) return null;
		if (name == 'Bool') return cast Bool;

		var phpClass = Boot.getPhpName(name);
		if (!Global.class_exists(phpClass)) return null;

		var hxClass = Boot.getClass(phpClass);

		return cast hxClass;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		if (String == cast cl) return args[0];

		var phpName = getPhpName(cl);
		if (phpName == null) return null;
		var nativeArgs:NativeArray = @:privateAccess args.arr;
		return Syntax.construct(phpName, Syntax.splat(nativeArgs));
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		if (String == cast cl) return cast '';
		if (Array == cast cl) return cast [];

		var phpName = getPhpName(cl);
		if (phpName == null) return null;

		var reflection = new ReflectionClass(phpName);
		return reflection.newInstanceWithoutConstructor();
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		if (e == null || constr == null) return null;

		var phpName = getPhpName(e);
		if (phpName == null) return null;

		if (!Global.in_array(constr, Syntax.staticCall(phpName, "__hx__list"))) {
			throw 'No such constructor $constr';
		}

		var paramsCounts:NativeAssocArray<Int> = Syntax.staticCall(phpName, "__hx__paramsCount");
		if ((params == null && paramsCounts[constr] != 0) || (params != null && params.length != paramsCounts[constr])) {
			throw 'Provided parameters count does not match expected parameters count';
		}

		if (params == null) {
			return Syntax.staticCall(phpName, constr);
		} else {
			var nativeArgs:NativeArray = @:privateAccess params.arr;
			return Syntax.staticCall(phpName, constr, Syntax.splat(nativeArgs));
		}
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		if (e == null || index == null) return null;

		var phpName = getPhpName(e);
		if (phpName == null) return null;

		var constructors:NativeIndexedArray<String> = Syntax.staticCall(phpName, "__hx__list");
		if (index < 0 || index >= Global.count(constructors)) {
			throw '$index is not a valid enum constructor index';
		}

		var constr = constructors[index];
		var paramsCounts:NativeAssocArray<Int> = Syntax.staticCall(phpName, "__hx__paramsCount");
		if ((params == null && paramsCounts[constr] != 0) || (params != null && params.length != paramsCounts[constr])) {
			throw 'Provided parameters count does not match expected parameters count';
		}

		if (params == null) {
			return Syntax.staticCall(phpName, constr);
		} else {
			var nativeArgs:NativeArray = @:privateAccess params.arr;
			return Syntax.staticCall(phpName, constr, Syntax.splat(nativeArgs));
		}
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		if (c == null) return null;
		if (c == String) {
			return [
				'substr', 'charAt', 'charCodeAt', 'indexOf',
				'lastIndexOf', 'split', 'toLowerCase',
				'toUpperCase', 'toString', 'length'
			];
		}

		var phpName = getPhpName(c);
		if (phpName == null) return null;

		var reflection = new ReflectionClass(phpName);

		var methods = new NativeArray();
		for (m in reflection.getMethods()) {
			var method:ReflectionMethod = m;
			if (!method.isStatic()) {
				var name = method.getName();
				if (!isServiceFieldName(name)) {
					methods.array_push(name);
				}
			}
		}

		var properties = new NativeArray();
		for (p in reflection.getProperties()) {
			var property:ReflectionProperty = p;
			if (!property.isStatic()) {
				var name = property.getName();
				if (!isServiceFieldName(name)) {
					properties.array_push(name);
				}
			}
		}
		properties = Global.array_diff(properties, methods);

		var fields = Global.array_merge(properties, methods);

		return @:privateAccess Array.wrap(fields);
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		if (c == null) return null;
		if (c == String) return ['fromCharCode'];

		var phpName = getPhpName(c);
		if (phpName == null) return null;

		var reflection = new ReflectionClass(phpName);

		var methods = new NativeArray();
		for (m in reflection.getMethods(ReflectionMethod.IS_STATIC)) {
			//TODO: report an issue on invalid type inference for iteration over `NativeIndexedArray`
			var m:ReflectionMethod = m;
			var name = m.getName();
			if (!isServiceFieldName(name) && phpName == m.getDeclaringClass().getName()) {
				methods.array_push(name);
			}
		}

		var properties = new NativeArray();
		for (p in reflection.getProperties(ReflectionProperty.IS_STATIC)) {
			var p:ReflectionMethod = p;
			var name = p.getName();
			if (!isServiceFieldName(name) && phpName == p.getDeclaringClass().getName()) {
				properties.array_push(name);
			}
		}
		properties = Global.array_diff(properties, methods);
		var fields = Global.array_merge(properties, methods);

		return @:privateAccess Array.wrap(fields);
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		if (e == null) return null;
		return @:privateAccess Array.wrap(untyped e.__hx__list());
	}

	public static function typeof( v : Dynamic ) : ValueType {
		if (v == null) return TNull;

		if (v.is_object()) {
			if (Reflect.isFunction(v)) return TFunction;
			if (Std.is(v, StdClass)) return TObject;
			if (Boot.isClass(v)) return TObject;

			var hxClass = Boot.getClass(Global.get_class(v));
			if (Boot.isEnumValue(v)) return TEnum(cast hxClass);
			return TClass(cast hxClass);
		}

		if (v.is_bool()) return TBool;
		if (v.is_int()) return TInt;
		if (v.is_float()) return TFloat;
		if (v.is_string()) return TClass(String);

		return TUnknown;
	}

	public static function enumEq<T:EnumValue>( a : T, b : T ) : Bool {
		if (a == b) return true;
		if (a == null || b == null) return false;

		try {
			if (Global.get_class(cast a) != Global.get_class(cast b)) return false;
			if (enumIndex(a) != enumIndex(b)) return false;

			var aParams:NativeIndexedArray<Dynamic> = untyped a.params;
			var bParams:NativeIndexedArray<Dynamic> = untyped b.params;
			for (i in 0...Global.count(aParams)) {
				//enums
				if (Boot.isEnumValue(aParams[i])) {
					if (!enumEq(aParams[i], bParams[i])) {
						return false;
					}
					continue;
				}
				//functions
				if (Reflect.isFunction(aParams[i])) {
					if (!Reflect.compareMethods(aParams[i], bParams[i])) {
						return false;
					}
					continue;
				}
				//everything else
				if (aParams[i] != bParams[i]) {
					return false;
				}
			}

			return true;
		} catch (e:Dynamic) {
			return false;
		}
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return untyped e.tag;
	}

	public inline static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return @:privateAccess Array.wrap(untyped e.params);
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		if (e == null) return null;

		var phpName = getPhpName(e);
		if (phpName == null) return null;

		var result = [];

		for (name in getEnumConstructs(e)) {
			var reflection = new ReflectionMethod(phpName, name);
			if (reflection.getNumberOfParameters() == 0) {
				result.push(reflection.invoke(null));
			}
		}

		return result;
	}

	/**
		Get corresponding PHP name for specified `type`.
		Returns `null` if `type` does not exist.
	**/
	static function getPhpName( type:EitherType<Class<Dynamic>,Enum<Dynamic>> ) : Null<String> {
		var haxeName = Boot.getHaxeName(cast type);

		return (haxeName == null ? null : Boot.getPhpName(haxeName));
	}

	/**
		check if specified `name` is a special field name generated by compiler.
	 **/
	static inline function isServiceFieldName(name:String) : Bool {
		return (name == '__construct' || name.indexOf('__hx__') == 0);
	}
}

