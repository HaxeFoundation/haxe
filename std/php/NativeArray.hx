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

package php;

import haxe.extern.EitherType;

using php.Global;

/**
	Native PHP array.
**/
@:coreType @:runtimeValue @:semantics(value) abstract NativeArray {
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
		return Global.array_values(this).iterator();

	public inline function keyValueIterator():NativeArrayKeyValueIterator
		return new NativeArrayKeyValueIterator(this);
}

private class NativeArrayKeyValueIterator {
	var length:Int;
	var current:Int = 0;
	var keys:NativeIndexedArray<EitherType<String, Int>>;
	var values:NativeIndexedArray<Dynamic>;

	public inline function new(data:NativeArray) {
		length = Global.count(data);
		this.keys = Global.array_keys(data);
		this.values = Global.array_values(data);
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():{key:EitherType<String, Int>, value:Dynamic} {
		return {key: keys[current], value: values[current++]};
	}
}
