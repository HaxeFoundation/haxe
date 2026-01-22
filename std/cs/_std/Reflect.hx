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
		if (getter != null && Std.isOfType(getter, HaxeFunction)) {
			return (cast getter : HaxeFunction).invoke();
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
		if (setter != null && Std.isOfType(setter, HaxeFunction)) {
			(cast setter : HaxeFunction).invoke1(value);
			return;
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

		// Numeric comparison - use __cs__ to avoid generic type cast issues
		if (Std.isOfType(a, Int) && Std.isOfType(b, Int)) {
			var ai:Int = untyped __cs__("(int)(object){0}", a);
			var bi:Int = untyped __cs__("(int)(object){0}", b);
			return ai < bi ? -1 : (ai > bi ? 1 : 0);
		}

		if (Std.isOfType(a, Float) && Std.isOfType(b, Float)) {
			var af:Float = untyped __cs__("(double)(object){0}", a);
			var bf:Float = untyped __cs__("(double)(object){0}", b);
			return af < bf ? -1 : (af > bf ? 1 : 0);
		}

		// String comparison - use String.CompareOrdinal for proper comparison
		if (Std.isOfType(a, String) && Std.isOfType(b, String)) {
			var result:Int = untyped __cs__("string.CompareOrdinal((string)(object){0}, (string)(object){1})", a, b);
			return result < 0 ? -1 : (result > 0 ? 1 : 0);
		}

		// Try IComparable
		var result:Int = untyped __cs__("((System.IComparable){0}).CompareTo({1})", a, b);
		return result;
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
		var dst:Dynamic = untyped __cs__("System.Activator.CreateInstance((System.Type){0})", nativeType);

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
