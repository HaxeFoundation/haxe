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
	var h:Dynamic;

	public function new():Void {
		h = php.Syntax.code("new \\WeakMap()");
	}

	public function set(key:K, value:V):Void {
		php.Syntax.code("{0}->offsetSet({1}, {2})", h, key, value);
	}

	public function get(key:K):Null<V> {
		if (!exists(key))
			return null;
		return php.Syntax.code("{0}->offsetGet({1})", h, key);
	}

	public function exists(key:K):Bool {
		return php.Syntax.code("{0}->offsetExists({1})", h, key);
	}

	public function remove(key:K):Bool {
		if (!exists(key))
			return false;
		php.Syntax.code("{0}->offsetUnset({1})", h, key);
		return true;
	}

	public function keys():Iterator<K> {
		var arr:php.NativeArray = php.Syntax.code("iterator_to_array({0}, false)", php.Syntax.code("(function() { foreach ({0} as $k => $v) { yield $k; } })()", h));
		return php.Lib.toHaxeArray(arr).iterator();
	}

	public function iterator():Iterator<V> {
		var arr:php.NativeArray = php.Syntax.code("iterator_to_array({0}, false)", php.Syntax.code("(function() { foreach ({0} as $v) { yield $v; } })()", h));
		return php.Lib.toHaxeArray(arr).iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<K, V> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():WeakMap<K, V> {
		var copied = new WeakMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keys();
		for (i in it) {
			s.add(Std.string(i));
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public function clear():Void {
		h = php.Syntax.code("new \\WeakMap()");
	}

	public function size():Int {
		return php.Syntax.code("count({0})", h);
	}
}
