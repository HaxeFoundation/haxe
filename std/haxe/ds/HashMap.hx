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

import haxe.iterators.HashMapKeyValueIterator;

/**
	HashMap allows mapping of hashable objects to arbitrary values.

	See `Map` for documentation details.

	@see https://haxe.org/manual/std-Map.html
**/
abstract HashMap<K:{function hashCode():Int;}, V>(HashMapData<K, V>) {
	/**
		Creates a new HashMap.
	**/
	public inline function new() {
		this = new HashMapData();
	}

	/**
		See `Map.set`
	**/
	@:arrayAccess public inline function set(k:K, v:V) {
		this.keys.set(k.hashCode(), k);
		this.values.set(k.hashCode(), v);
	}

	/**
		See `Map.get`
	**/
	@:arrayAccess public inline function get(k:K) {
		return this.values.get(k.hashCode());
	}

	/**
		See `Map.exists`
	**/
	public inline function exists(k:K) {
		return this.values.exists(k.hashCode());
	}

	/**
		See `Map.remove`
	**/
	public inline function remove(k:K) {
		this.values.remove(k.hashCode());
		return this.keys.remove(k.hashCode());
	}

	/**
		See `Map.keys`
	**/
	public inline function keys() {
		return this.keys.iterator();
	}

	/**
		See `Map.copy`
	**/
	public function copy():HashMap<K, V> {
		var copied = new HashMapData();
		copied.keys = this.keys.copy();
		copied.values = this.values.copy();
		return cast copied;
	}

	/**
		See `Map.iterator`
	**/
	public inline function iterator() {
		return this.values.iterator();
	}

	/**
		See `Map.keyValueIterator`
	**/
	public inline function keyValueIterator():HashMapKeyValueIterator<K, V> {
		return new HashMapKeyValueIterator(cast this);
	}

	/**
		See `Map.clear`
	**/
	public inline function clear():Void {
		this.keys.clear();
		this.values.clear();
	}
}

private class HashMapData<K:{function hashCode():Int;}, V> {
	public var keys:IntMap<K>;
	public var values:IntMap<V>;

	public inline function new() {
		keys = new IntMap();
		values = new IntMap();
	}
}
