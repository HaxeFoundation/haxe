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

package haxe.ds;

/**
	WeakMap allows mapping of object keys to arbitrary values.

	The keys are considered to be weak references on static targets.

	See `Map` for documentation details.

	@see https://haxe.org/manual/std-Map.html
**/
class WeakMap<K: { },V> implements haxe.Constraints.IMap<K,V> {

	/**
		Creates a new WeakMap.
	**/
	public function new():Void {
		throw "Not implemented for this platform";
	}

	/**
		See `Map.set`
	**/
	public function set(key:K, value:V):Void {
	}

	/**
		See `Map.get`
	**/
	public function get(key:K):Null<V> {
		return null;
	}

	/**
		See `Map.exists`
	**/
	public function exists(key:K):Bool {
		return false;
	}

	/**
		See `Map.remove`
	**/
	public function remove(key:K):Bool {
		return false;
	}

	/**
		See `Map.keys`
	**/
	public function keys():Iterator<K> {
		return null;
	}

	/**
		See `Map.iterator`
	**/
	public function iterator():Iterator<V> {
		return null;
	}

	/**
		See `Map.toString`
	**/
	public function toString():String {
		return null;
	}
}