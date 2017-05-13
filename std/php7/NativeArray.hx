/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package php;

using php.Global;

/**
	Native PHP array.
**/
@:coreType @:runtimeValue abstract NativeArray {
	public inline function new()
		this = Syntax.arrayDecl();

	@:arrayAccess function getByInt(key:Int):Dynamic;
	@:arrayAccess function setByInt(key:Int, val:Dynamic):Dynamic;
	@:arrayAccess function getByFloat(key:Float):Dynamic;
	@:arrayAccess function setByFloat(key:Float, val:Dynamic):Dynamic;
	@:arrayAccess function getByString(key:String):Dynamic;
	@:arrayAccess function setByString(key:String, val:Dynamic):Dynamic;
	@:arrayAccess function getByBool(key:Bool):Dynamic;
	@:arrayAccess function setByBool(key:Bool, val:Dynamic):Dynamic;

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
