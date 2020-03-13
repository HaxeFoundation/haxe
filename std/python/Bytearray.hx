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

package python;

import python.Syntax;

@:native("bytearray")
extern class Bytearray implements ArrayAccess<Int> {
	var length(get, never):Int;

	@:overload(function():Void {})
	@:overload(function(it:Array<Int>):Void {})
	@:overload(function(it:NativeIterable<Int>):Void {})
	@:overload(function(size:Int):Void {})
	function new(source:String, encoding:String, ?errors:Dynamic):Void;

	private inline function get_length():Int {
		return python.internal.UBuiltins.len(this);
	}

	function append(x:Int):Void;
	function extend(t:Bytearray):Void;

	inline function get(i:Int):Int {
		return Syntax.arrayAccess(this, i);
	}

	inline function set(i:Int, v:Int):Void {
		this.__setitem__(i, v);
	}

	function __setitem__(i:Int, v:Int):Void;

	function decode(encoding:String = "utf-8", errors:String = "strict"):String;
}
