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

package cs;

/**
 * Runtime helper for C# target.
 * Provides dynamic type conversions, field access, and function invocation.
 */
@:keep
@:native("haxe.lang.Runtime")
class Cs {
	/**
	 * Call a function dynamically with the given arguments.
	 * Handles both HaxeFunction subclasses and C# delegates.
	 */
	public static function call(func:Dynamic, args:Array<Dynamic>):Dynamic {
		if (func == null) {
			throw "Cannot call null function";
		}

		// If it's a HaxeFunction, use invokeDynamic
		if (Std.isOfType(func, HaxeFunction)) {
			return (cast func : HaxeFunction).invokeDynamic(args);
		}

		// Otherwise, try to use C# reflection to invoke the delegate
		return untyped __cs__("haxe.lang.Runtime.InvokeDelegate({0}, {1})", func, args);
	}

	/**
	 * Convert a dynamic value to int. Returns 0 for null.
	 */
	public static function dynamicToInt(d:Dynamic):Int {
		if (d == null)
			return 0;
		if (Std.isOfType(d, Int))
			return cast d;
		return untyped __cs__("System.Convert.ToInt32({0})", d);
	}

	/**
	 * Convert a dynamic value to float. Returns 0.0 for null.
	 */
	public static function dynamicToDouble(d:Dynamic):Float {
		if (d == null)
			return 0.0;
		if (Std.isOfType(d, Float))
			return cast d;
		return untyped __cs__("System.Convert.ToDouble({0})", d);
	}

	/**
	 * Convert a dynamic value to bool. Returns false for null.
	 */
	public static function dynamicToBool(d:Dynamic):Bool {
		if (d == null)
			return false;
		if (Std.isOfType(d, Bool))
			return cast d;
		return untyped __cs__("System.Convert.ToBoolean({0})", d);
	}

	/**
	 * Read a field from an object dynamically.
	 */
	public static function readField(obj:Dynamic, name:String):Dynamic {
		if (obj == null) {
			throw "Cannot read field from null";
		}

		// Try HaxeDynamicObject first
		if (Std.isOfType(obj, HaxeDynamicObject)) {
			return (cast obj : HaxeDynamicObject)._hx_getField(name);
		}

		// Use reflection for other objects
		return untyped __cs__("haxe.lang.Runtime.GetField({0}, {1})", obj, name);
	}

	/**
	 * Write a field to an object dynamically.
	 */
	public static function writeField(obj:Dynamic, name:String, value:Dynamic):Void {
		if (obj == null) {
			throw "Cannot write field to null";
		}

		// Try HaxeDynamicObject first
		if (Std.isOfType(obj, HaxeDynamicObject)) {
			(cast obj : HaxeDynamicObject)._hx_setField(name, value);
			return;
		}

		// Use reflection for other objects
		untyped __cs__("haxe.lang.Runtime.SetField({0}, {1}, {2})", obj, name, value);
	}

	/**
	 * Check if an object is a function (HaxeFunction or C# delegate).
	 */
	public static function isFunction(obj:Dynamic):Bool {
		if (obj == null)
			return false;
		if (Std.isOfType(obj, HaxeFunction))
			return true;
		// Check if it's a C# delegate
		return untyped __cs__("{0} is System.Delegate", obj);
	}
}
