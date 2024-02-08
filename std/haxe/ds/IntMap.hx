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

package haxe.ds;

/**
	IntMap allows mapping of Int keys to arbitrary values.

	See `Map` for documentation details.

	@see https://haxe.org/manual/std-Map.html
**/
extern class IntMap<T> implements haxe.Constraints.IMap<Int, T> {
	/**
		Creates a new IntMap.
	**/
	function new():Void;

	/**
		See `Map.set`
	**/
	function set(key:Int, value:T):Void;

	/**
		See `Map.get`
	**/
	function get(key:Int):Null<T>;

	/**
		See `Map.exists`
	**/
	function exists(key:Int):Bool;

	/**
		See `Map.remove`
	**/
	function remove(key:Int):Bool;

	/**
		See `Map.keys`

		(java) Implementation detail: Do not `set()` any new value while
		iterating, as it may cause a resize, which will break iteration.
	**/
	function keys():Iterator<Int>;

	/**
		See `Map.iterator`

		(java) Implementation detail: Do not `set()` any new value while
		iterating, as it may cause a resize, which will break iteration.
	**/
	function iterator():Iterator<T>;

	/**
		See `Map.keyValueIterator`
	**/
	#if eval
	@:runtime inline function keyValueIterator():KeyValueIterator<Int, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}
	#else
	function keyValueIterator():KeyValueIterator<Int, T>;
	#end

	/**
		See `Map.copy`
	**/
	function copy():IntMap<T>;

	/**
		See `Map.toString`
	**/
	function toString():String;

	/**
		See `Map.clear`
	**/
	function clear():Void;
}
