/*
 * Copyright (C)2005-2018 Haxe Foundation
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
abstract NativeIndexedArray<T>(NativeArray) from NativeArray to NativeArray {
	public inline function new()
		this = Syntax.arrayDecl();

	@:arrayAccess
	inline function get(idx:Int):T
		return this[idx];

	@:arrayAccess
	inline function set(idx:Int, val:T):T
		return this[idx] = val;

	public inline function push(val:T)
		Syntax.code('{0}[] = {1}', this, val);

	public inline function iterator():NativeIndexedArrayIterator<T>
		return new NativeIndexedArrayIterator(this);

	public inline function keyValueIterator():NativeIndexedArrayKeyValueIterator<T>
		return new NativeIndexedArrayKeyValueIterator(this);

	@:to
	inline function toHaxeArray():Array<T>
		return @:privateAccess Array.wrap(this);

	@:from
	static inline function fromHaxeArray<T>(a:Array<T>):NativeIndexedArray<T>
		return @:privateAccess a.arr;
}

private class NativeIndexedArrayIterator<T> {
	var length:Int;
	var current:Int = 0;
	var data:NativeIndexedArray<T>;

	public inline function new(data:NativeIndexedArray<T>) {
		length = Global.count(data);
		this.data = data;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():T {
		return data[current++];
	}
}

private class NativeIndexedArrayKeyValueIterator<T> {
	var length:Int;
	var current:Int = 0;
	var data:NativeIndexedArray<T>;

	public inline function new(data:NativeIndexedArray<T>) {
		length = Global.count(data);
		this.data = data;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:data[current++]};
	}
}