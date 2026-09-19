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

@:coreApi(check = Off)
class WeakMap<K:{}, V> implements haxe.Constraints.IMap<K, V> {
	var h:js.lib.WeakMap<V>;

	public inline function new():Void {
		h = new js.lib.WeakMap();
	}

	public inline function set(key:K, value:V):Void {
		h.set(cast key, value);
	}

	public inline function get(key:K):Null<V> {
		return h.get(cast key);
	}

	public inline function exists(key:K):Bool {
		return h.has(cast key);
	}

	public inline function remove(key:K):Bool {
		return h.delete(cast key);
	}

	public function keys():Iterator<K> {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}

	public function iterator():Iterator<V> {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}

	public inline function keyValueIterator():KeyValueIterator<K, V> {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}

	public function copy():WeakMap<K, V> {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}

	public function toString():String {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}

	public inline function clear():Void {
		h = new js.lib.WeakMap();
	}

	public function size():Int {
		throw new haxe.exceptions.NotImplementedException("JS WeakMaps do not support enumeration");
	}
}
