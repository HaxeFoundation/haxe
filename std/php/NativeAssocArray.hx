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

@:forward
@:semantics(value)
abstract NativeAssocArray<T>(NativeArray) from NativeArray to NativeArray {
	public inline function new()
		this = Syntax.arrayDecl();

	@:arrayAccess
	inline function get(key:String):T
		return this[key];

	@:arrayAccess
	inline function set(key:String, val:T):T
		return this[key] = val;

	public inline function iterator()
		return (cast Global.array_values(this) : NativeIndexedArray<T>).iterator();

	public inline function keyValueIterator():NativeAssocArrayKeyValueIterator<T>
		return new NativeAssocArrayKeyValueIterator(this);
}

private class NativeAssocArrayKeyValueIterator<T> {
	var length:Int;
	var current:Int = 0;
	var keys:NativeIndexedArray<String>;
	var values:NativeIndexedArray<T>;

	public inline function new(data:NativeIndexedArray<T>) {
		length = Global.count(data);
		this.keys = Global.array_keys(data);
		this.values = cast Global.array_values(data);
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():{key:String, value:T} {
		return {key: keys[current], value: values[current++]};
	}
}
