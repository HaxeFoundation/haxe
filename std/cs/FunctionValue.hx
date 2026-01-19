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

import cs.Int64;

/**
 * Lightweight value wrapper for function arguments and return values without boxing.
 * Stack-allocated struct that can hold primitives (via prim field) or
 * references (via obj field) with a kind discriminator.
 *
 * This struct enables zero-allocation function calls by:
 * - Storing primitives directly in the 'prim' field (no boxing)
 * - Storing reference types in the 'obj' field
 * - Using 'kind' discriminator to determine which field contains the value
 *
 * kind values:
 * - 0 = No value (missing optional argument, or void return)
 * - 1 = Object value in obj field (prim is 0)
 * - 2 = Primitive value in prim field (obj is null)
 */
@:keep
@:native("haxe.lang.FunctionValue")
extern class FunctionValue {
	/**
	 * Object slot for reference types.
	 */
	var obj:Dynamic;

	/**
	 * Primitive slot - stores int, long, float, double, bool via bit conversion.
	 * Using long (64-bit) allows storing all primitive types without precision loss.
	 */
	var prim:Int64;

	/**
	 * Discriminator for which field contains the value:
	 * 0 = no value (missing/void), 1 = object in obj, 2 = primitive in prim
	 */
	var kind:Int;

	// Static factory methods
	static function FromInt(value:Int):FunctionValue;
	static function FromLong(value:Int64):FunctionValue;
	static function FromDouble(value:Float):FunctionValue;
	static function FromFloat(value:Single):FunctionValue;
	static function FromBool(value:Bool):FunctionValue;
	static function FromObject(value:Dynamic):FunctionValue;
	static function Missing():FunctionValue;

	// Extraction methods
	function ToInt():Int;
	function ToLong():Int64;
	function ToDouble():Float;
	function ToFloat():Single;
	function ToBool():Bool;
	function ToStringValue():String;
	function ToDynamic():Dynamic;

	/**
	 * Check if this FunctionValue contains a value (kind != 0)
	 */
	var HasValue(default, null):Bool;
}
