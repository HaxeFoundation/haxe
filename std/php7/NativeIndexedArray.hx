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

import haxe.extern.EitherType;

@:forward
abstract NativeIndexedArray<T>(NativeArray) from NativeArray to NativeArray {
	public inline function new()
		this = untyped __php__("[]");

	@:arrayAccess
	inline function get(idx:Int):T
		return this[idx];

	@:arrayAccess
	inline function set(idx:Int, val:T)
		this[idx] = val;

	@:to
	inline function toHaxeArray():Array<T>
		return @:privateAccess Array.wrap(this);

	@:from
	static inline function fromHaxeArray<T>(a:Array<T>):NativeIndexedArray<T>
		return @:privateAccess a.arr;

	public inline function filter(cb:T->Bool):NativeIndexedArray<T>
		return Global.array_filter(this, cb);

	public inline function map<S>(cb:T->S):NativeIndexedArray<S>
		return Global.array_map(cb, this);

	public inline function pop():T
		return Global.array_pop(this);

	public inline function push(val:T):Int
		return Global.array_push(this, val);

	public inline function search(needle:T):KeyOrFalse
		return Global.array_search(needle, this, true);

	public inline function shift():T
		return Global.array_shift(this);

	public inline function splice(offset:Int, len:Int, ?repl:T):NativeIndexedArray<T>
		return Global.array_splice(this, offset, len, repl);

	public inline function unshift(val:T):Int
		return Global.array_unshift(this, val);

	public inline function usort(func:T->T->Int):Bool
		return Global.usort(this, func);
}

private typedef KeyOrFalse = EitherType<EitherType<String,Int>,Bool>;
