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
	Represents a C# native array (`T[]`)
**/
@:nativeGen extern class NativeArray<T> implements ArrayAccess<T> {
	var length(default, null):Int;

	function new(size:Int):Void;

	@:arrayAccess function get(index:Int):T;
	@:arrayAccess function set(index:Int, value:T):T;

	public static inline function arraycopy<T>(src:NativeArray<T>, srcPos:Int, dest:NativeArray<T>, destPos:Int, length:Int):Void {
		untyped __cs__("System.Array.Copy({0}, {1}, {2}, {3}, {4})", src, srcPos, dest, destPos, length);
	}
}
