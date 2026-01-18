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
 * Lightweight argument wrapper for function invocation without boxing.
 * Stack-allocated struct that can hold primitives (via prim field) or
 * references (via obj field) with optional parameter tracking.
 */
@:keep
@:native("haxe.lang.FunctionArg")
extern class FunctionArg {
	/**
	 * Object slot for reference types and boxed values.
	 */
	var obj:Dynamic;

	/**
	 * Primitive slot - stores int, long, float, double, bool via bit conversion.
	 */
	var prim:Int64;

	/**
	 * Whether this argument was provided (for optional parameter support).
	 */
	var hasValue:Bool;

	// Static factory methods
	static function FromInt(value:Int):FunctionArg;
	static function FromLong(value:Int64):FunctionArg;
	static function FromDouble(value:Float):FunctionArg;
	static function FromFloat(value:Single):FunctionArg;
	static function FromBool(value:Bool):FunctionArg;
	static function FromObject(value:Dynamic):FunctionArg;
	static function Missing():FunctionArg;

	// Extraction methods
	function ToInt():Int;
	function ToLong():Int64;
	function ToDouble():Float;
	function ToFloat():Single;
	function ToBool():Bool;
	function ToStringValue():String;
	function ToDynamic():Dynamic;
}
