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
 * Provides dynamic type conversions, field access, function invocation,
 * and other utility functions needed by generated code.
 *
 * This class is marked with @:keep to ensure DCE doesn't remove it
 * when functions are called from inline C# code (__cs__).
 *
 * All runtime helper functions should be placed here, NOT in cs.Boot.
 * cs.Boot is only for initialization code that runs at startup.
 */
@:keep
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

	/**
	 * Convert any object to string representation.
	 */
	public static function toString(obj:Dynamic):String {
		if (obj == null) {
			return "null";
		}
		return untyped __cs__("{0}.ToString()", obj);
	}

	/**
	 * Parse a string to integer with the given radix.
	 */
	public static function parseInt(s:String, radix:Int):Int {
		return untyped __cs__("int.Parse({0}, {1} == 16 ? System.Globalization.NumberStyles.HexNumber : System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture)", s, radix);
	}

	/**
	 * Parse a string to floating point number.
	 */
	public static function parseFloat(s:String):Float {
		return untyped __cs__("double.Parse({0}, System.Globalization.CultureInfo.InvariantCulture)", s);
	}

	/**
	 * Output a trace message to the console.
	 */
	public static function trace(v:Dynamic, ?infos:haxe.PosInfos):Void {
		var str = toString(v);
		if (infos != null) {
			str = untyped __cs__("((haxe.root.HaxeDynamicObject){0})._hx_getField(\"fileName\")", infos) + ":"
				+ untyped __cs__("((haxe.root.HaxeDynamicObject){0})._hx_getField(\"lineNumber\")", infos) + ": " + str;
		}
		untyped __cs__("System.Console.WriteLine({0})", str);
	}

	private static var _random:Dynamic = null;

	/**
	 * Generate a random number between 0 and 1.
	 */
	public static function random():Float {
		if (_random == null) {
			_random = untyped __cs__("new System.Random()");
		}
		return untyped __cs__("((System.Random){0}).NextDouble()", _random);
	}

	// =====================================================================
	// Dynamic Operations (for runtime dispatch when types are unknown)
	// These mirror jvm.Jvm operations for consistency across targets
	// =====================================================================

	/**
	 * Dynamic addition: handles string concatenation and numeric addition.
	 */
	public static function opAdd(a:Dynamic, b:Dynamic):Dynamic {
		if (Std.isOfType(a, String) || Std.isOfType(b, String)) {
			return toString(a) + toString(b);
		}
		if (Std.isOfType(a, Float) || Std.isOfType(b, Float)) {
			return dynamicToDouble(a) + dynamicToDouble(b);
		}
		return dynamicToInt(a) + dynamicToInt(b);
	}

	/**
	 * Dynamic subtraction.
	 */
	public static function opSub(a:Dynamic, b:Dynamic):Dynamic {
		if (Std.isOfType(a, Float) || Std.isOfType(b, Float)) {
			return dynamicToDouble(a) - dynamicToDouble(b);
		}
		return dynamicToInt(a) - dynamicToInt(b);
	}

	/**
	 * Dynamic multiplication.
	 */
	public static function opMul(a:Dynamic, b:Dynamic):Dynamic {
		if (Std.isOfType(a, Float) || Std.isOfType(b, Float)) {
			return dynamicToDouble(a) * dynamicToDouble(b);
		}
		return dynamicToInt(a) * dynamicToInt(b);
	}

	/**
	 * Dynamic division.
	 */
	public static function opDiv(a:Dynamic, b:Dynamic):Dynamic {
		// Division always returns Float
		return dynamicToDouble(a) / dynamicToDouble(b);
	}

	/**
	 * Dynamic modulo.
	 */
	public static function opMod(a:Dynamic, b:Dynamic):Dynamic {
		if (Std.isOfType(a, Float) || Std.isOfType(b, Float)) {
			return dynamicToDouble(a) % dynamicToDouble(b);
		}
		return dynamicToInt(a) % dynamicToInt(b);
	}

	/**
	 * Dynamic bitwise AND.
	 */
	public static function opAnd(a:Dynamic, b:Dynamic):Dynamic {
		return dynamicToInt(a) & dynamicToInt(b);
	}

	/**
	 * Dynamic bitwise OR.
	 */
	public static function opOr(a:Dynamic, b:Dynamic):Dynamic {
		return dynamicToInt(a) | dynamicToInt(b);
	}

	/**
	 * Dynamic bitwise XOR.
	 */
	public static function opXor(a:Dynamic, b:Dynamic):Dynamic {
		return dynamicToInt(a) ^ dynamicToInt(b);
	}

	/**
	 * Dynamic left shift.
	 */
	public static function opShl(a:Dynamic, b:Dynamic):Dynamic {
		return dynamicToInt(a) << dynamicToInt(b);
	}

	/**
	 * Dynamic right shift (arithmetic).
	 */
	public static function opShr(a:Dynamic, b:Dynamic):Dynamic {
		return dynamicToInt(a) >> dynamicToInt(b);
	}

	/**
	 * Dynamic unsigned right shift.
	 */
	public static function opUshr(a:Dynamic, b:Dynamic):Dynamic {
		return untyped __cs__("(int)((uint){0} >> {1})", dynamicToInt(a), dynamicToInt(b));
	}

	/**
	 * Dynamic negation.
	 */
	public static function opNeg(a:Dynamic):Dynamic {
		if (Std.isOfType(a, Float)) {
			return -dynamicToDouble(a);
		}
		return -dynamicToInt(a);
	}

	/**
	 * Dynamic bitwise complement.
	 */
	public static function opNegBits(a:Dynamic):Dynamic {
		return ~dynamicToInt(a);
	}

	/**
	 * Dynamic increment.
	 */
	public static function opIncrement(a:Dynamic):Dynamic {
		if (Std.isOfType(a, Float)) {
			return dynamicToDouble(a) + 1.0;
		}
		return dynamicToInt(a) + 1;
	}

	/**
	 * Dynamic decrement.
	 */
	public static function opDecrement(a:Dynamic):Dynamic {
		if (Std.isOfType(a, Float)) {
			return dynamicToDouble(a) - 1.0;
		}
		return dynamicToInt(a) - 1;
	}

	/**
	 * String comparison (like Java's compareTo).
	 */
	public static function stringCompare(v1:String, v2:String):Int {
		if (v1 == null) {
			return v2 == null ? 0 : 1;
		}
		if (v2 == null) {
			return -1;
		}
		return untyped __cs__("string.Compare({0}, {1}, System.StringComparison.Ordinal)", v1, v2);
	}

	/**
	 * General comparison for dynamic values.
	 */
	public static function compare(a:Dynamic, b:Dynamic):Int {
		return Reflect.compare(a, b);
	}
}
