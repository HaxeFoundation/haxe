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

@:coreApi
class Reflect {
	public static function hasField(o:Dynamic, field:String):Bool {
		if (o == null)
			return false;

		// Check HaxeDynamicObject first
		if (Std.isOfType(o, HaxeDynamicObject)) {
			return (cast o : HaxeDynamicObject)._hx_hasField(field);
		}

		// Check HaxeObject - use _hx_getFields to check
		if (Std.isOfType(o, HaxeObject)) {
			var hxObj:HaxeObject = cast o;
			var fields = hxObj._hx_getFields();
			return fields.indexOf(field) >= 0;
		}

		// Check if o is a System.Type (for static field access)
		var isType:Bool = untyped __cs__("{0} is System.Type", o);
		if (isType) {
			// AOT-safe: Use registered static field accessor
			return untyped __cs__("global::haxe.lang.HaxeReflection.hasField((System.Type){0}, {1})", o, field);
		}

		// Use reflection for other objects
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", o);
		var fieldInfo:Dynamic = untyped __cs__("((System.Type){0}).GetField({1})", nativeType, field);
		if (fieldInfo != null)
			return true;
		var propInfo:Dynamic = untyped __cs__("((System.Type){0}).GetProperty({1})", nativeType, field);
		if (propInfo != null)
			return true;
		var methodInfo:Dynamic = untyped __cs__("((System.Type){0}).GetMethod({1})", nativeType, field);
		return methodInfo != null;
	}

	public static function field(o:Dynamic, field:String):Dynamic {
		if (o == null)
			return null;

		// Check HaxeDynamicObject first
		if (Std.isOfType(o, HaxeDynamicObject)) {
			return (cast o : HaxeDynamicObject)._hx_getField(field);
		}

		// Check HaxeObject
		if (Std.isOfType(o, HaxeObject)) {
			return (cast o : HaxeObject)._hx_getField(field);
		}

		// Special handling for strings (native System.String)
		if (Std.isOfType(o, String)) {
			var str:String = cast o;
			return switch (field) {
				case "length": str.length;
				case "charAt": getStringMethodClosure(str, 0);
				case "charCodeAt": getStringMethodClosure(str, 1);
				case "indexOf": getStringMethodClosure(str, 2);
				case "lastIndexOf": getStringMethodClosure(str, 3);
				case "split": getStringMethodClosure(str, 4);
				case "substr": getStringMethodClosure(str, 5);
				case "substring": getStringMethodClosure(str, 6);
				case "toLowerCase": getStringMethodClosure(str, 7);
				case "toUpperCase": getStringMethodClosure(str, 8);
				case "toString": getStringMethodClosure(str, 9);
				default: null;
			};
		}

		// Check if o is a System.Type (for static field access)
		var isType:Bool = untyped __cs__("{0} is System.Type", o);
		if (isType) {
			// AOT-safe: Use registered static field accessor
			return untyped __cs__("global::haxe.lang.HaxeReflection.getField((System.Type){0}, {1})", o, field);
		}

		// Use reflection for other objects
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", o);

		// Try field first
		var fieldInfo:Dynamic = untyped __cs__("((System.Type){0}).GetField({1})", nativeType, field);
		if (fieldInfo != null) {
			return untyped __cs__("((System.Reflection.FieldInfo){0}).GetValue({1})", fieldInfo, o);
		}

		// Try property
		var propInfo:Dynamic = untyped __cs__("((System.Type){0}).GetProperty({1})", nativeType, field);
		if (propInfo != null) {
			return untyped __cs__("((System.Reflection.PropertyInfo){0}).GetValue({1})", propInfo, o);
		}

		// Try method (return as closure)
		var methodInfo:Dynamic = untyped __cs__("((System.Type){0}).GetMethod({1})", nativeType, field);
		if (methodInfo != null) {
			// Create a wrapper function for the method
			return createMethodClosure(o, methodInfo);
		}

		return null;
	}

	public static function setField(o:Dynamic, field:String, value:Dynamic):Void {
		if (o == null)
			return;

		// Check HaxeDynamicObject first
		if (Std.isOfType(o, HaxeDynamicObject)) {
			(cast o : HaxeDynamicObject)._hx_setField(field, value);
			return;
		}

		// Check HaxeObject
		if (Std.isOfType(o, HaxeObject)) {
			(cast o : HaxeObject)._hx_setField(field, value);
			return;
		}

		// Use reflection for other objects
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", o);

		// Try field first
		var fieldInfo:Dynamic = untyped __cs__("((System.Type){0}).GetField({1})", nativeType, field);
		if (fieldInfo != null) {
			untyped __cs__("((System.Reflection.FieldInfo){0}).SetValue({1}, {2})", fieldInfo, o, value);
			return;
		}

		// Try property
		var propInfo:Dynamic = untyped __cs__("((System.Type){0}).GetProperty({1})", nativeType, field);
		if (propInfo != null) {
			untyped __cs__("((System.Reflection.PropertyInfo){0}).SetValue({1}, {2})", propInfo, o, value);
			return;
		}
	}

	public static function getProperty(o:Dynamic, field:String):Dynamic {
		if (o == null)
			return null;

		// Check HaxeDynamicObject - no properties, just fields
		if (Std.isOfType(o, HaxeDynamicObject)) {
			return (cast o : HaxeDynamicObject)._hx_getField(field);
		}

		// Try to call getter first
		var getter = Reflect.field(o, "get_" + field);
		if (getter != null) {
			var isFunc:Bool = untyped __cs__("{0} is haxe.lang.Function", getter);
			if (isFunc) {
				return untyped __cs__("((haxe.lang.Function){0}).__hx_invoke0().ToDynamic()", getter);
			}
		}

		// Fall back to field access
		return Reflect.field(o, field);
	}

	public static function setProperty(o:Dynamic, field:String, value:Dynamic):Void {
		if (o == null)
			return;

		// Check HaxeDynamicObject - no properties, just fields
		if (Std.isOfType(o, HaxeDynamicObject)) {
			(cast o : HaxeDynamicObject)._hx_setField(field, value);
			return;
		}

		// Try to call setter first
		var setter = Reflect.field(o, "set_" + field);
		if (setter != null) {
			var isFunc:Bool = untyped __cs__("{0} is haxe.lang.Function", setter);
			if (isFunc) {
				untyped __cs__("((haxe.lang.Function){0}).__hx_invoke1(haxe.lang.Value.FromObject({1}))", setter, value);
				return;
			}
		}

		// Fall back to field access
		Reflect.setField(o, field, value);
	}

	public static function callMethod(o:Dynamic, func:haxe.Constraints.Function, args:Array<Dynamic>):Dynamic {
		if (func == null)
			return null;

		// If it's a HaxeFunction, use invokeDynamic
		if (Std.isOfType(func, HaxeFunction)) {
			return (cast func : HaxeFunction).invokeDynamic(args);
		}

		// If it's a native delegate, invoke it
		if (untyped __cs__("{0} is System.Delegate", func)) {
			var nativeArgs:Dynamic = untyped __cs__("new object[{0}]", args.length);
			for (i in 0...args.length) {
				untyped __cs__("((object[]){0})[{1}] = {2}", nativeArgs, i, args[i]);
			}
			return untyped __cs__("((System.Delegate){0}).DynamicInvoke((object[]){1})", func, nativeArgs);
		}

		return null;
	}

	public static function fields(o:Dynamic):Array<String> {
		if (o == null)
			return [];

		// Check HaxeDynamicObject first
		if (Std.isOfType(o, HaxeDynamicObject)) {
			return (cast o : HaxeDynamicObject)._hx_getFields();
		}

		// Check HaxeObject
		if (Std.isOfType(o, HaxeObject)) {
			return (cast o : HaxeObject)._hx_getFields();
		}

		// Use reflection for other objects
		var result:Array<String> = [];
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", o);

		// Get fields
		var fieldInfos:Dynamic = untyped __cs__("((System.Type){0}).GetFields(System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public)", nativeType);
		var fieldCount:Int = untyped __cs__("((System.Reflection.FieldInfo[]){0}).Length", fieldInfos);
		for (i in 0...fieldCount) {
			var fieldInfo:Dynamic = untyped __cs__("((System.Reflection.FieldInfo[]){0})[{1}]", fieldInfos, i);
			var name:String = untyped __cs__("((System.Reflection.FieldInfo){0}).Name", fieldInfo);
			if (!StringTools.startsWith(name, "_hx_"))
				result.push(name);
		}

		return result;
	}

	public static function isFunction(f:Dynamic):Bool {
		if (f == null)
			return false;
		if (Std.isOfType(f, HaxeFunction))
			return true;
		return untyped __cs__("{0} is System.Delegate", f);
	}

	// Note: Uses Dynamic instead of generic T for type erasure compatibility.
	// With type erasure, T would become object anyway, and C# can't infer
	// generic type parameters from object-typed arguments.
	public static function compare<T>(a:T, b:T):Int {
		return compareImpl(a, b);
	}

	private static function compareImpl(a:Dynamic, b:Dynamic):Int {
		if (a == b)
			return 0;
		if (a == null)
			return -1;
		if (b == null)
			return 1;

		// Numeric comparison - check Float BEFORE Int because Std.isOfType(2.0, Int)
		// returns true for integral doubles, but we can't directly unbox a boxed double as int.
		// Using Convert.ToDouble handles all numeric types safely.
		if (Std.isOfType(a, Float) && Std.isOfType(b, Float)) {
			var af:Float = untyped __cs__("System.Convert.ToDouble({0})", a);
			var bf:Float = untyped __cs__("System.Convert.ToDouble({0})", b);
			return af < bf ? -1 : (af > bf ? 1 : 0);
		}

		// String comparison - use String.CompareOrdinal for proper comparison
		if (Std.isOfType(a, String) && Std.isOfType(b, String)) {
			var result:Int = untyped __cs__("string.CompareOrdinal((string)(object){0}, (string)(object){1})", a, b);
			return result < 0 ? -1 : (result > 0 ? 1 : 0);
		}

		// Try IComparable - use safe pattern matching to avoid cast exception
		// (Null<T> should never be boxed in object; if it is, fix the code generator)
		if (untyped __cs__("{0} is System.IComparable", a)) {
			var result:Int = untyped __cs__("((System.IComparable){0}).CompareTo({1})", a, b);
			return result;
		}
		return 0;
	}

	public static function compareMethods(f1:Dynamic, f2:Dynamic):Bool {
		if (f1 == f2)
			return true;
		if (f1 == null || f2 == null)
			return false;

		// Compare HaxeFunctions by reference
		if (Std.isOfType(f1, HaxeFunction) && Std.isOfType(f2, HaxeFunction)) {
			return f1 == f2;
		}

		// Compare delegates
		if (untyped __cs__("{0} is System.Delegate", f1) && untyped __cs__("{0} is System.Delegate", f2)) {
			return untyped __cs__("object.Equals({0}, {1})", f1, f2);
		}

		return false;
	}

	public static function isObject(v:Dynamic):Bool {
		if (v == null)
			return false;
		// Not an object if it's a primitive
		if (Std.isOfType(v, Bool))
			return false;
		if (Std.isOfType(v, Int))
			return false;
		if (Std.isOfType(v, Float))
			return false;
		// Functions are not objects
		if (isFunction(v))
			return false;
		// Enum values are not objects
		if (isEnumValue(v))
			return false;
		return true;
	}

	public static function isEnumValue(v:Dynamic):Bool {
		if (v == null)
			return false;
		// Check if the object has _hx_index field (all enum values have it)
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", v);
		var indexField:Dynamic = untyped __cs__("((System.Type){0}).GetField(\"_hx_index\")", nativeType);
		return indexField != null;
	}

	public static function deleteField(o:Dynamic, field:String):Bool {
		if (o == null)
			return false;

		// Only works on HaxeDynamicObject
		if (Std.isOfType(o, HaxeDynamicObject)) {
			return (cast o : HaxeDynamicObject)._hx_deleteField(field);
		}

		// Can't delete fields from regular objects
		return false;
	}

	public static function copy<T>(o:Null<T>):Null<T> {
		if (o == null)
			return null;

		// Copy HaxeDynamicObject
		if (Std.isOfType(o, HaxeDynamicObject)) {
			var src = cast(o, HaxeDynamicObject);
			var dst = new HaxeDynamicObject();
			for (field in src._hx_getFields()) {
				dst._hx_setField(field, src._hx_getField(field));
			}
			return cast dst;
		}

		// For other objects, do shallow copy via reflection
		var nativeType:Dynamic = untyped __cs__("((object){0}).GetType()", o);

		// Try AOT-safe HaxeReflection.createEmpty first (works for Haxe classes)
		var dst:Dynamic = untyped __cs__("global::haxe.lang.HaxeReflection.createEmpty((System.Type){0})", nativeType);

		// Fallback to Activator for native C# classes
		if (dst == null) {
			try {
				dst = untyped __cs__("System.Activator.CreateInstance((System.Type){0})", nativeType);
			} catch (e:Dynamic) {
				// CreateInstance failed (can happen in AOT), return null
				return null;
			}
		}

		// Safety check: if we couldn't create an instance, return null
		if (dst == null)
			return null;

		var fieldInfos:Dynamic = untyped __cs__("((System.Type){0}).GetFields(System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public)", nativeType);
		var fieldCount:Int = untyped __cs__("((System.Reflection.FieldInfo[]){0}).Length", fieldInfos);
		for (i in 0...fieldCount) {
			var fieldInfo:Dynamic = untyped __cs__("((System.Reflection.FieldInfo[]){0})[{1}]", fieldInfos, i);
			var value:Dynamic = untyped __cs__("((System.Reflection.FieldInfo){0}).GetValue({1})", fieldInfo, o);
			untyped __cs__("((System.Reflection.FieldInfo){0}).SetValue({1}, {2})", fieldInfo, dst, value);
		}

		return cast dst;
	}

	public static function makeVarArgs<T>(f:Array<Dynamic>->T):Dynamic {
		return new VarArgsFunction(f);
	}

	private static function createMethodClosure(obj:Dynamic, methodInfo:Dynamic):Dynamic {
		return new MethodClosure(obj, methodInfo);
	}

	// String method closure cache: ConditionalWeakTable<string, Function[]>
	// Caches StringMethodFunction instances per string to avoid allocation on each Reflect.field() call
	static var stringMethodCache:Dynamic = untyped __cs__("new System.Runtime.CompilerServices.ConditionalWeakTable<string, haxe.lang.Function[]>()");

	private static function getStringMethodClosure(str:String, methodIndex:Int):Dynamic {
		// Type alias for readability
		var cwt:Dynamic = stringMethodCache;

		// Try to get existing cache entry
		var cache:Dynamic = null;
		var found:Bool = untyped __cs__("((System.Runtime.CompilerServices.ConditionalWeakTable<string, haxe.lang.Function[]>){0}).TryGetValue({1}, out haxe.lang.Function[] arr)", cwt, str);
		if (found) {
			cache = untyped __cs__("arr");
		} else {
			// Create new array and add to cache
			cache = untyped __cs__("new haxe.lang.Function[10]");
			untyped __cs__("((System.Runtime.CompilerServices.ConditionalWeakTable<string, haxe.lang.Function[]>){0}).Add({1}, (haxe.lang.Function[]){2})", cwt, str, cache);
		}

		// Check if this method is already cached
		var existing:Dynamic = untyped __cs__("((haxe.lang.Function[]){0})[{1}]", cache, methodIndex);
		if (existing != null)
			return existing;

		// Create and cache
		var closure = new StringMethodFunction(str, methodIndex);
		untyped __cs__("((haxe.lang.Function[]){0})[{1}] = {2}", cache, methodIndex, closure);
		return closure;
	}
}

private class VarArgsFunction extends HaxeFunction {
	var func:Array<Dynamic>->Dynamic;

	public function new(f:Array<Dynamic>->Dynamic) {
		this.func = f;
	}

	override public function invokeDynamic(args:Array<Dynamic>):Dynamic {
		return func(args);
	}
}

private class MethodClosure extends HaxeFunction {
	var obj:Dynamic;
	var methodInfo:Dynamic;

	public function new(obj:Dynamic, methodInfo:Dynamic) {
		this.obj = obj;
		this.methodInfo = methodInfo;
	}

	override public function invokeDynamic(args:Array<Dynamic>):Dynamic {
		var nativeArgs:Dynamic = untyped __cs__("new object[{0}]", args.length);
		for (i in 0...args.length) {
			untyped __cs__("((object[]){0})[{1}] = {2}", nativeArgs, i, args[i]);
		}
		return untyped __cs__("((System.Reflection.MethodInfo){0}).Invoke({1}, (object[]){2})", methodInfo, obj, nativeArgs);
	}
}

private class StringMethodFunction extends HaxeFunction {
	var str:String;
	var methodIndex:Int;

	public function new(str:String, methodIndex:Int) {
		this.str = str;
		this.methodIndex = methodIndex;
	}

	override public function invokeDynamic(args:Array<Dynamic>):Dynamic {
		var len = args != null ? args.length : 0;
		return switch (methodIndex) {
			case 0: cs.StringExt.charAt(str, args != null ? args[0] : 0);
			case 1: cs.StringExt.charCodeAt(str, args != null ? args[0] : 0);
			case 2: cs.StringExt.indexOf(str, args != null ? args[0] : "", len > 1 ? args[1] : null);
			case 3: cs.StringExt.lastIndexOf(str, args != null ? args[0] : "", len > 1 ? args[1] : null);
			case 4: cs.StringExt.split(str, args != null ? args[0] : "");
			case 5: cs.StringExt.substr(str, args != null ? args[0] : 0, len > 1 ? args[1] : null);
			case 6: cs.StringExt.substring(str, args != null ? args[0] : 0, len > 1 ? args[1] : null);
			case 7: untyped __cs__("{0}.ToLower()", str);
			case 8: untyped __cs__("{0}.ToUpper()", str);
			case 9: str;
			default: null;
		};
	}
}
