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

import cs.HaxeDynamicObject;
import cs.HaxeFunction;
import cs.HaxeObject;

enum ValueType {
	TNull;
	TInt;
	TInt64;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi
class Type {
	public static function getClass<T>(o:T):Null<Class<T>> {
		if (o == null)
			return null;
		// Classes and enums are not instances
		if (Std.isOfType(o, Class) || Std.isOfType(o, Enum))
			return null;
		// Dynamic objects don't have a class
		if (Std.isOfType(o, HaxeDynamicObject))
			return null;
		// Check if it's an enum value (has _hx_index field)
		if (isEnumValue(o))
			return null;
		// Return the class - GetType() returns System.Type which is Class<T> in Haxe
		return untyped __cs__("((object){0}).GetType()", o);
	}

	public static function getEnum(o:EnumValue):Null<Enum<Dynamic>> {
		if (o == null)
			return null;
		// Check if it's an enum value by checking for _hx_index
		if (!isEnumValue(o))
			return null;
		// Get the base type (the enum type is the superclass)
		return untyped __cs__("((object){0}).GetType().BaseType", o);
	}

	public static function getSuperClass(c:Class<Dynamic>):Null<Class<Dynamic>> {
		if (c == null)
			return null;
		// String and basic types don't have a Haxe superclass
		if (c == cast String)
			return null;
		// c is System.Type - access BaseType directly
		var baseType:Class<Dynamic> = untyped __cs__("((System.Type){0}).BaseType", c);
		if (baseType == null)
			return null;
		// Don't return System.Object as superclass
		var baseTypeName:String = untyped __cs__("((System.Type){0}).FullName", baseType);
		if (baseTypeName == "System.Object" || baseTypeName == "haxe.root.HaxeObject")
			return null;
		return baseType;
	}

	public static function getClassName(c:Class<Dynamic>):String {
		if (c == null)
			return null;
		// c is already System.Type in C# - access FullName directly
		var name:String = untyped __cs__("((System.Type){0}).FullName", c);
		// Remove haxe.root. prefix
		if (name.indexOf("haxe.root.") == 0)
			return name.substr(10);
		// Handle System.String -> String
		if (name == "System.String")
			return "String";
		return name;
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		if (e == null)
			return null;
		// e is already System.Type in C# - access FullName directly
		var name:String = untyped __cs__("((System.Type){0}).FullName", e);
		// Remove haxe.root. prefix
		if (name.indexOf("haxe.root.") == 0)
			return name.substr(10);
		return name;
	}

	public static function resolveClass(name:String):Null<Class<Dynamic>> {
		if (name == null)
			return null;
		// Try common mappings first
		if (name == "String")
			return cast String;
		// Try with haxe.root prefix
		var fullName = name.indexOf(".") == -1 ? "haxe.root." + name : name;
		var nativeType:Class<Dynamic> = untyped __cs__("System.Type.GetType({0})", fullName);
		if (nativeType != null)
			return nativeType;
		// Try without prefix
		nativeType = untyped __cs__("System.Type.GetType({0})", name);
		return nativeType;
	}

	public static function resolveEnum(name:String):Null<Enum<Dynamic>> {
		// Same as resolveClass but returns as Enum
		var c = resolveClass(name);
		return cast c;
	}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		if (cl == null)
			return null;
		var argLen = (args == null) ? 0 : args.length;
		// Build native args array
		var nativeArgs:Dynamic = untyped __cs__("new object[{0}]", argLen);
		for (i in 0...argLen) {
			untyped __cs__("((object[]){0})[{1}] = {2}", nativeArgs, i, args[i]);
		}
		// Use AOT-safe registry-based factory (falls back to Activator for non-Haxe types)
		return cast untyped __cs__("global::haxe.lang.HaxeStaticFields.create((System.Type){0}, (object[]){1})", cl, nativeArgs);
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		if (cl == null)
			return null;
		// Use AOT-safe registry-based factory (falls back to GetUninitializedObject for non-Haxe types)
		return cast untyped __cs__("global::haxe.lang.HaxeStaticFields.createEmpty((System.Type){0})", cl);
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		if (e == null)
			return null;
		if (params == null || params.length == 0) {
			// For parameterless constructors, try the parent enum type's static field first.
			// This is more AOT-friendly: public static fields are preserved by the trimmer.
			var parentField:Dynamic = untyped __cs__("((System.Type){0}).GetField({1}, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)", e, constr);
			if (parentField != null) {
				return cast untyped __cs__("((System.Reflection.FieldInfo){0}).GetValue(null)", parentField);
			}
		}
		// Fall through to nested type approach for parameterized constructors or if parent field not found
		var nestedTypes:Dynamic = untyped __cs__("((System.Type){0}).GetNestedTypes()", e);
		var nestedCount:Int = untyped __cs__("((System.Type[]){0}).Length", nestedTypes);
		for (i in 0...nestedCount) {
			var nested:Dynamic = untyped __cs__("((System.Type[]){0})[{1}]", nestedTypes, i);
			var nestedName:String = untyped __cs__("((System.Type){0}).Name", nested);
			// Match both "Name" and "Name_Impl_" for singleton enum constructors
			var matchedName = nestedName;
			if (StringTools.endsWith(nestedName, "_Impl_")) {
				matchedName = nestedName.substr(0, nestedName.length - 6);
			}
			if (matchedName == constr) {
				if (params == null || params.length == 0) {
					// Try to get singleton instance from nested type
					var instanceField:Dynamic = untyped __cs__("((System.Type){0}).GetField(\"Instance\", System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)", nested);
					if (instanceField != null) {
						return cast untyped __cs__("((System.Reflection.FieldInfo){0}).GetValue(null)", instanceField);
					}
					try {
						return cast untyped __cs__("System.Activator.CreateInstance((System.Type){0})", nested);
					} catch (d:Dynamic) {
						return null;
					}
				} else {
					// Extract the actual array from the optional parameter to avoid Null<Array> issues
					var paramsArray:Array<Dynamic> = params;
					var nativeArgs:Dynamic = untyped __cs__("new object[{0}]", paramsArray.length);
					for (j in 0...paramsArray.length) {
						untyped __cs__("((object[]){0})[{1}] = {2}", nativeArgs, j, paramsArray[j]);
					}
					try {
						return cast untyped __cs__("System.Activator.CreateInstance((System.Type){0}, (object[]){1})", nested, nativeArgs);
					} catch (d:Dynamic) {
						return null;
					}
				}
			}
		}
		return null;
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		var constructs = getEnumConstructs(e);
		if (index < 0 || index >= constructs.length)
			return null;
		return createEnum(e, constructs[index], params);
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		if (c == null)
			return [];
		// Walk up the class hierarchy collecting instance field names from the registry
		var result:Array<String> = [];
		var current = c;
		while (current != null) {
			var names:cs.NativeArray<String> = untyped __cs__("global::haxe.lang.HaxeStaticFields.getInstanceFieldNames((System.Type){0})", current);
			if (names != null) {
				for (i in 0...names.length) {
					if (result.indexOf(names[i]) == -1)
						result.push(names[i]);
				}
			}
			current = getSuperClass(current);
		}
		return result;
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		if (c == null)
			return [];

		// Try AOT-safe registry first
		var fieldNames:cs.NativeArray<String> = untyped __cs__("global::haxe.lang.HaxeStaticFields.getClassFieldNames((System.Type){0})", c);
		if (fieldNames != null) {
			var result:Array<String> = [];
			for (i in 0...fieldNames.length) {
				result.push(fieldNames[i]);
			}
			return result;
		}

		// Fallback to reflection (works in JIT, may fail in AOT)
		var result:Array<String> = [];
		// c is System.Type - get static fields directly
		var fields:Dynamic = untyped __cs__("((System.Type){0}).GetFields(System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public)", c);
		var fieldCount:Int = untyped __cs__("((System.Reflection.FieldInfo[]){0}).Length", fields);
		for (i in 0...fieldCount) {
			var field:Dynamic = untyped __cs__("((System.Reflection.FieldInfo[]){0})[{1}]", fields, i);
			var name:String = untyped __cs__("((System.Reflection.FieldInfo){0}).Name", field);
			if (!StringTools.startsWith(name, "_hx_"))
				result.push(name);
		}
		// Get static properties (C# auto-properties are generated for Haxe static fields)
		var properties:Dynamic = untyped __cs__("((System.Type){0}).GetProperties(System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public)", c);
		var propCount:Int = untyped __cs__("((System.Reflection.PropertyInfo[]){0}).Length", properties);
		for (i in 0...propCount) {
			var prop:Dynamic = untyped __cs__("((System.Reflection.PropertyInfo[]){0})[{1}]", properties, i);
			var name:String = untyped __cs__("((System.Reflection.PropertyInfo){0}).Name", prop);
			// Skip internal properties
			if (!StringTools.startsWith(name, "_hx_")) {
				if (result.indexOf(name) == -1)
					result.push(name);
			}
		}
		// Get static methods
		var methods:Dynamic = untyped __cs__("((System.Type){0}).GetMethods(System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public)", c);
		var methodCount:Int = untyped __cs__("((System.Reflection.MethodInfo[]){0}).Length", methods);
		for (i in 0...methodCount) {
			var method:Dynamic = untyped __cs__("((System.Reflection.MethodInfo[]){0})[{1}]", methods, i);
			var name:String = untyped __cs__("((System.Reflection.MethodInfo){0}).Name", method);
			if (!StringTools.startsWith(name, "_hx_")) {
				if (result.indexOf(name) == -1)
					result.push(name);
			}
		}
		return result;
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		if (e == null)
			return [];

		// Try registry first (AOT-safe, correct declaration order)
		var names:cs.NativeArray<String> = untyped __cs__("global::haxe.lang.HaxeStaticFields.getEnumConstructs((System.Type){0})", e);
		if (names != null) {
			var result = new Array<String>();
			for (i in 0...names.length)
				result.push(names[i]);
			return result;
		}

		// Fallback: reflection (may not preserve declaration order in AOT)
		var result:Array<String> = [];
		var nestedTypes:Dynamic = untyped __cs__("((System.Type){0}).GetNestedTypes()", e);
		var nestedCount:Int = untyped __cs__("((System.Type[]){0}).Length", nestedTypes);
		for (i in 0...nestedCount) {
			var nested:Dynamic = untyped __cs__("((System.Type[]){0})[{1}]", nestedTypes, i);
			var name:String = untyped __cs__("((System.Type){0}).Name", nested);
			// Strip _Impl_ suffix for singleton enum constructors
			if (StringTools.endsWith(name, "_Impl_")) {
				name = name.substr(0, name.length - 6);
			}
			result.push(name);
		}
		return result;
	}

	public static function typeof(v:Dynamic):ValueType {
		if (v == null)
			return TNull;
		// Check for boolean first
		if (Std.isOfType(v, Bool))
			return TBool;
		// Check for int
		if (Std.isOfType(v, Int))
			return TInt;
		// Check for float
		if (Std.isOfType(v, Float))
			return TFloat;
		// Check for functions
		if (Std.isOfType(v, HaxeFunction))
			return TFunction;
		if (untyped __cs__("{0} is System.Delegate", v))
			return TFunction;
		// Check for dynamic objects (anonymous)
		if (Std.isOfType(v, HaxeDynamicObject))
			return TObject;
		// Check for enum values
		if (isEnumValue(v)) {
			var e = getEnum(cast v);
			return TEnum(e);
		}
		// Must be a class instance
		var c = getClass(v);
		if (c != null)
			return TClass(c);
		return TUnknown;
	}

	public static function enumEq<T:EnumValue>(a:T, b:T):Bool {
		if (a == null)
			return b == null;
		if (b == null)
			return false;
		// Check if same index
		var aIndex:Int = enumIndex(a);
		var bIndex:Int = enumIndex(b);
		if (aIndex != bIndex)
			return false;
		// Check parameters
		var aParams = enumParameters(a);
		var bParams = enumParameters(b);
		if (aParams.length != bParams.length)
			return false;
		for (i in 0...aParams.length) {
			if (!enumValueEq(aParams[i], bParams[i]))
				return false;
		}
		return true;
	}

	private static function enumValueEq(a:Dynamic, b:Dynamic):Bool {
		if (a == b)
			return true;
		if (a == null || b == null)
			return false;
		if (isEnumValue(a) && isEnumValue(b))
			return enumEq(cast a, cast b);
		// Use Equals for value comparison (== on boxed primitives does reference comparison)
		return untyped __cs__("System.Object.Equals({0}, {1})", a, b);
	}

	public static function enumConstructor(e:EnumValue):String {
		if (e == null)
			return null;
		// Get the class name which is the constructor name
		var name:String = untyped __cs__("((object){0}).GetType().Name", e);
		// Strip _Impl_ suffix for singleton enum constructors
		if (StringTools.endsWith(name, "_Impl_")) {
			return name.substr(0, name.length - 6);
		}
		return name;
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		if (e == null)
			return [];
		var result:Array<Dynamic> = [];
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", e);
		// Get public instance fields (these are the parameters)
		var fields:Dynamic = untyped __cs__("((System.Type){0}).GetFields(System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public)", nativeType);
		var fieldCount:Int = untyped __cs__("((System.Reflection.FieldInfo[]){0}).Length", fields);
		for (i in 0...fieldCount) {
			var field:Dynamic = untyped __cs__("((System.Reflection.FieldInfo[]){0})[{1}]", fields, i);
			var name:String = untyped __cs__("((System.Reflection.FieldInfo){0}).Name", field);
			// Skip internal fields
			if (name != "_hx_index") {
				var value:Dynamic = untyped __cs__("((System.Reflection.FieldInfo){0}).GetValue({1})", field, e);
				result.push(value);
			}
		}
		return result;
	}

	public static function enumIndex(e:EnumValue):Int {
		if (e == null)
			return -1;
		// Use IHaxeEnum interface for AOT-safe enum index access
		if (untyped __cs__("{0} is global::haxe.lang.IHaxeEnum", e)) {
			return untyped __cs__("((global::haxe.lang.IHaxeEnum){0})._hx_getIndex()", e);
		}
		// Fallback: reflection for non-Haxe enums
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", e);
		var indexField:Dynamic = untyped __cs__(
			"((System.Type){0}).GetField(\"_hx_index\", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public)",
			nativeType);
		if (indexField != null) {
			return untyped __cs__("(int)((System.Reflection.FieldInfo){0}).GetValue({1})", indexField, e);
		}
		return 0;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		if (e == null)
			return [];
		var result:Array<T> = [];
		var constructs = getEnumConstructs(e);
		for (name in constructs) {
			var v:Dynamic = createEnum(e, name);
			// Only include parameterless constructors (singleton instances)
			if (v != null)
				result.push(v);
		}
		return result;
	}

	private static function isEnumValue(v:Dynamic):Bool {
		if (v == null)
			return false;
		// Check if the object has _hx_index field (all enum values have it)
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", v);
		var indexField:Dynamic = untyped __cs__("((System.Type){0}).GetField(\"_hx_index\")", nativeType);
		return indexField != null;
	}
}
