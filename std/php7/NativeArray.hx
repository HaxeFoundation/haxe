/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package php7;

using php7.Global;


/**
	Native PHP array.
**/
@:coreType @:runtimeValue abstract NativeArray {
	public inline function new()
		this = untyped __php__("[]");

	@:arrayAccess function get(key:Scalar):Dynamic;
	@:arrayAccess function set(key:Scalar, val:Dynamic):Dynamic;

	public inline function iterator()
		return new NativeArrayIterator(this);
}

/**
	Allows iterating over native PHP array with Haxe for-loop
**/
private class NativeArrayIterator {
	var arr:NativeArray;
	var hasMore:Bool;

	public inline function new( a:NativeArray ) {
		arr = a;
		hasMore = (arr.reset() != false);
	}

	public inline function hasNext() : Bool {
		return hasMore;
	}

	public inline function next() : Dynamic {
		var result = arr.current();
		hasMore = (arr.next() != false);
		return result;
	}
}
