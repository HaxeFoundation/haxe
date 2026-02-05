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
 * High-level runtime helpers for Haxe semantic operations on Dynamic types.
 *
 * This class implements Haxe language behavior when operating on Dynamic values,
 * written in Haxe for maintainability by Haxe developers.
 *
 * Responsibilities:
 * - Dynamic arithmetic (opAdd, opSub, opMul, opDiv, opMod)
 * - Bitwise and unary operations (opAnd, opOr, opXor, opShl, opShr, opUshr, opNeg, opNegBits, opNot)
 * - Dynamic array/field access (arrayGet, arraySet)
 * - Compound assignments (fieldAddAssign, arrayMulAssign, etc.)
 * - Increment/decrement operations (fieldPostIncrement, arrayPreDecrement, etc.)
 * - Comparison helpers (compare, stringCompare)
 * - Utilities (trace, random, parseInt, parseFloat)
 *
 * For low-level C# runtime support (type conversions, AOT-safe reflection),
 * see haxe.lang.Runtime which is written in C# for .NET compatibility.
 *
 * This class is marked with @:keep to ensure DCE doesn't remove it
 * when functions are called from inline C# code (__cs__).
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
	 * Note: Similar to haxe.lang.Runtime.toInt but callable from Haxe code.
	 * Used internally by dynamic operations (opAdd, opMul, etc.).
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
	 * Note: Similar to haxe.lang.Runtime.toDouble but callable from Haxe code.
	 * Used internally by dynamic operations (opAdd, opMul, etc.).
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
	 * Note: Similar to haxe.lang.Runtime.toBool but callable from Haxe code.
	 * Used internally by dynamic operations and generated condition code.
	 */
	public static function dynamicToBool(d:Dynamic):Bool {
		if (d == null)
			return false;
		if (Std.isOfType(d, Bool))
			return cast d;
		return untyped __cs__("System.Convert.ToBoolean({0})", d);
	}

	/**
	 * Convert any object to string representation.
	 * Uses invariant culture for floats/doubles to ensure consistent decimal separator.
	 * For HaxeObject instances, calls the virtual toString() method.
	 */
	public static function toString(obj:Dynamic):String {
		if (obj == null) {
			return "null";
		}
		// Handle bool specially for Haxe compatibility (lowercase "true"/"false")
		if (untyped __cs__("{0} is bool", obj)) {
			return untyped __cs__("((bool){0}) ? \"true\" : \"false\"", obj);
		}
		// Handle floats/doubles with invariant culture to ensure '.' decimal separator
		if (untyped __cs__("{0} is double", obj)) {
			return untyped __cs__("((double){0}).ToString(System.Globalization.CultureInfo.InvariantCulture)", obj);
		}
		if (untyped __cs__("{0} is float", obj)) {
			return untyped __cs__("((float){0}).ToString(System.Globalization.CultureInfo.InvariantCulture)", obj);
		}
		// For HaxeObject instances, call the virtual toString() method directly
		// This handles Array, custom classes with toString(), etc. via dynamic dispatch
		if (untyped __cs__("{0} is global::haxe.lang.HaxeObject hxObj", obj)) {
			return untyped __cs__("hxObj.toString()");
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
			str = untyped __cs__("((haxe.lang.HaxeDynamicObject){0})._hx_getField(\"fileName\")", infos) + ":"
				+ untyped __cs__("((haxe.lang.HaxeDynamicObject){0})._hx_getField(\"lineNumber\")", infos) + ": " + str;
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
	 * Dynamic boolean NOT.
	 */
	public static function opNot(a:Dynamic):Dynamic {
		return !dynamicToBool(a);
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

	// =====================================================================
	// Dynamic Array/Indexer Access
	// =====================================================================

	/**
	 * Get element from a dynamic array or indexable object.
	 */
	public static function arrayGet(obj:Dynamic, index:Int):Dynamic {
		if (obj == null) {
			throw "Cannot index null";
		}
		// Try as native C# array first
		if (untyped __cs__("{0} is System.Array", obj)) {
			return untyped __cs__("((System.Array){0}).GetValue({1})", obj, index);
		}
		// Try as Haxe Array
		if (Std.isOfType(obj, Array)) {
			return (cast obj : Array<Dynamic>)[index];
		}
		// Fallback: throw - we can't handle other indexable types without reflection
		throw "Cannot index object of type " + Type.getClassName(Type.getClass(obj));
	}

	/**
	 * Set element in a dynamic array or indexable object.
	 */
	public static function arraySet(obj:Dynamic, index:Int, value:Dynamic):Dynamic {
		if (obj == null) {
			throw "Cannot index null";
		}
		// Try as native C# array first
		if (untyped __cs__("{0} is System.Array", obj)) {
			untyped __cs__("((System.Array){0}).SetValue({1}, {2})", obj, value, index);
			return value;
		}
		// Try as Haxe Array
		if (Std.isOfType(obj, Array)) {
			(cast obj : Array<Dynamic>)[index] = value;
			return value;
		}
		// Fallback: throw - we can't handle other indexable types without reflection
		throw "Cannot set index on object of type " + Type.getClassName(Type.getClass(obj));
	}

	// =====================================================================
	// Dynamic Field Read-Modify-Write Operations
	// These helpers perform increment/decrement/compound assignment on
	// dynamic fields, handling the read-modify-write pattern that C# can't
	// express with ((int)Reflect.field(obj, name))++
	// =====================================================================

	/**
	 * Increment a dynamic field by 1 and return the OLD value (postfix ++).
	 */
	public static function fieldPostIncrement(obj:Dynamic, field:String):Dynamic {
		var oldVal = Reflect.field(obj, field);
		Reflect.setField(obj, field, opIncrement(oldVal));
		return oldVal;
	}

	/**
	 * Increment a dynamic field by 1 and return the NEW value (prefix ++).
	 */
	public static function fieldPreIncrement(obj:Dynamic, field:String):Dynamic {
		var newVal = opIncrement(Reflect.field(obj, field));
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Decrement a dynamic field by 1 and return the OLD value (postfix --).
	 */
	public static function fieldPostDecrement(obj:Dynamic, field:String):Dynamic {
		var oldVal = Reflect.field(obj, field);
		Reflect.setField(obj, field, opDecrement(oldVal));
		return oldVal;
	}

	/**
	 * Decrement a dynamic field by 1 and return the NEW value (prefix --).
	 */
	public static function fieldPreDecrement(obj:Dynamic, field:String):Dynamic {
		var newVal = opDecrement(Reflect.field(obj, field));
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Compound add-assign on a dynamic field: field += value.
	 * The originalValue parameter ensures correct evaluation order - it must be
	 * read BEFORE the addend is evaluated (which may have side effects).
	 */
	public static function fieldAddAssign(obj:Dynamic, field:String, originalValue:Dynamic, addend:Dynamic):Dynamic {
		var newVal = opAdd(originalValue, addend);
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Compound sub-assign on a dynamic field: field -= value.
	 * The originalValue parameter ensures correct evaluation order.
	 */
	public static function fieldSubAssign(obj:Dynamic, field:String, originalValue:Dynamic, subtrahend:Dynamic):Dynamic {
		var newVal = opSub(originalValue, subtrahend);
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Compound mul-assign on a dynamic field: field *= value.
	 * The originalValue parameter ensures correct evaluation order.
	 */
	public static function fieldMulAssign(obj:Dynamic, field:String, originalValue:Dynamic, multiplier:Dynamic):Dynamic {
		var newVal = opMul(originalValue, multiplier);
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Compound div-assign on a dynamic field: field /= value.
	 * The originalValue parameter ensures correct evaluation order.
	 */
	public static function fieldDivAssign(obj:Dynamic, field:String, originalValue:Dynamic, divisor:Dynamic):Dynamic {
		var newVal = opDiv(originalValue, divisor);
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	/**
	 * Compound mod-assign on a dynamic field: field %= value.
	 * The originalValue parameter ensures correct evaluation order.
	 */
	public static function fieldModAssign(obj:Dynamic, field:String, originalValue:Dynamic, divisor:Dynamic):Dynamic {
		var newVal = opMod(originalValue, divisor);
		Reflect.setField(obj, field, newVal);
		return newVal;
	}

	// =====================================================================
	// Dynamic Array Index Read-Modify-Write Operations
	// Same pattern as field operations, but for array/indexer access.
	// =====================================================================

	/**
	 * Increment array element by 1 and return the OLD value (postfix ++).
	 */
	public static function arrayPostIncrement(arr:Dynamic, index:Int):Dynamic {
		var oldVal = arrayGet(arr, index);
		arraySet(arr, index, opIncrement(oldVal));
		return oldVal;
	}

	/**
	 * Increment array element by 1 and return the NEW value (prefix ++).
	 */
	public static function arrayPreIncrement(arr:Dynamic, index:Int):Dynamic {
		var newVal = opIncrement(arrayGet(arr, index));
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Decrement array element by 1 and return the OLD value (postfix --).
	 */
	public static function arrayPostDecrement(arr:Dynamic, index:Int):Dynamic {
		var oldVal = arrayGet(arr, index);
		arraySet(arr, index, opDecrement(oldVal));
		return oldVal;
	}

	/**
	 * Decrement array element by 1 and return the NEW value (prefix --).
	 */
	public static function arrayPreDecrement(arr:Dynamic, index:Int):Dynamic {
		var newVal = opDecrement(arrayGet(arr, index));
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Compound add-assign on array element: arr[i] += value.
	 */
	public static function arrayAddAssign(arr:Dynamic, index:Int, value:Dynamic):Dynamic {
		var newVal = opAdd(arrayGet(arr, index), value);
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Compound sub-assign on array element: arr[i] -= value.
	 */
	public static function arraySubAssign(arr:Dynamic, index:Int, value:Dynamic):Dynamic {
		var newVal = opSub(arrayGet(arr, index), value);
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Compound mul-assign on array element: arr[i] *= value.
	 */
	public static function arrayMulAssign(arr:Dynamic, index:Int, value:Dynamic):Dynamic {
		var newVal = opMul(arrayGet(arr, index), value);
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Compound div-assign on array element: arr[i] /= value.
	 */
	public static function arrayDivAssign(arr:Dynamic, index:Int, value:Dynamic):Dynamic {
		var newVal = opDiv(arrayGet(arr, index), value);
		arraySet(arr, index, newVal);
		return newVal;
	}

	/**
	 * Compound mod-assign on array element: arr[i] %= value.
	 */
	public static function arrayModAssign(arr:Dynamic, index:Int, value:Dynamic):Dynamic {
		var newVal = opMod(arrayGet(arr, index), value);
		arraySet(arr, index, newVal);
		return newVal;
	}
}
