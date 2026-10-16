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

	public inline function new():Void {
		h = lua.Syntax.code("setmetatable({}, {__mode = 'k'})");
	}

	public inline function set(key:K, value:V):Void {
		untyped h[key] = value;
	}

	public inline function get(key:K):Null<V> {
		return untyped h[key];
	}

	public inline function exists(key:K):Bool {
		return untyped h[key] != null;
	}

	public function remove(key:K):Bool {
		untyped {
			if (h[key] == null)
				return false;
			h[key] = null;
			return true;
		}
	}

	public function keys():Iterator<K>
		untyped {
			var cur = next(h, null);
			return {
				next: function() {
					var ret = cur;
					cur = untyped next(h, cur);
					return ret;
				},
				hasNext: function() return cur != null
			}
		}

	public function iterator():Iterator<V> {
		var itr = keys();
		return untyped {
			hasNext: itr.hasNext,
			next: function() return h[itr.next()]
		};
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

	public inline function clear():Void {
		h = lua.Syntax.code("setmetatable({}, {__mode = 'k'})");
	}

	public function size():Int {
		return lua.Syntax.code("(function() local s = 0; for _ in pairs({0}) do s = s + 1 end; return s end)()", h);
	}
}
