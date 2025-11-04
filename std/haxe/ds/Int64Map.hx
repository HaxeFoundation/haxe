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
	Int64Map allows mapping of Int64 keys to arbitrary values.

	See `Map` for documentation details.

	@see https://haxe.org/manual/std-Map.html
**/
class Int64Map<T> implements haxe.Constraints.IMap<haxe.Int64, T> {

	var m : Map<String,T>;

	/**
		Creates a new Int64Map.
	**/
	public function new():Void {
		m = new Map();
	}

	/**
		See `Map.set`
	**/
	public function set(key:Int64, value:T):Void {
		m.set(key.toString(),value);
	}

	/**
		See `Map.get`
	**/
	public function get(key:Int64):Null<T> {
		return m.get(key.toString());
	}

	/**
		See `Map.exists`
	**/
	public function exists(key:Int64):Bool {
		return m.exists(key.toString());
	}

	/**
		See `Map.remove`
	**/
	public function remove(key:Int64):Bool {
		return m.remove(key.toString());
	}

	/**
		See `Map.keys`

		(java) Implementation detail: Do not `set()` any new value while
		iterating, as it may cause a resize, which will break iteration.
	**/
	public function keys():Iterator<Int64> {
		var it = m.keys();
		return {
			hasNext : () -> it.hasNext(),
			next: () -> {
				return haxe.Int64.parseString(it.next());
			}
		};
	}

	/**
		See `Map.iterator`

		(java) Implementation detail: Do not `set()` any new value while
		iterating, as it may cause a resize, which will break iteration.
	**/
	public function iterator():Iterator<T> {
		return m.iterator();
	}

	/**
		See `Map.keyValueIterator`
	**/
	public function keyValueIterator():KeyValueIterator<Int64, T> {
		var it = m.keyValueIterator();
		return {
			hasNext : () -> it.hasNext(),
			next : () -> {
				var v = it.next();
				return { key : haxe.Int64.parseString(v.key), value : v.value };
			}
		};
	}

	/**
		See `Map.copy`
	**/
	public function copy():Int64Map<T> {
		var v = new Int64Map();
		v.m = m.copy();
		return v;
	}

	/**
		See `Map.toString`
	**/
	public function toString():String {
		return m.toString();
	}

	/**
		See `Map.clear`
	**/
	public function clear():Void {
		m.clear();
	}

	public function size():Int {
		return m.size();
	}

}
