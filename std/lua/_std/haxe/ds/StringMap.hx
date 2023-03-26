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

import lua.Lua;

class StringMap<T> implements haxe.Constraints.IMap<String, T> {
	private var h:lua.Table<String, T>;

	static var tnull:Dynamic = lua.Table.create();

	public inline function new():Void {
		h = lua.Table.create();
	}

	public inline function set(key:String, value:T):Void
		untyped {
			if (value == null) {
				h[key] = tnull;
			} else {
				h[key] = value;
			}
		}

	public inline function get(key:String):Null<T>
		untyped {
			var ret = h[key];
			if (ret == tnull) {
				return null;
			}
			return ret;
		}

	public inline function exists(key:String):Bool
		untyped {
			return h[key] != null;
		}

	public function remove(key:String):Bool
		untyped {
			if (h[key] == null) {
				return false;
			} else {
				h[key] = null;
				return true;
			}
		}

	public function keys():Iterator<String> {
		var next = Lua.next;
		var cur = next(h, null).index;
		return {
			next: function() {
				var ret = cur;
				cur = next(h, cur).index;
				return cast ret;
			},
			hasNext: function() return cur != null
		}
	}

	public function iterator():Iterator<T> {
		var it = keys();
		return untyped {
			hasNext: function() return it.hasNext(),
			next: function() return h[it.next()]
		};
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():StringMap<T> {
		var copied = new StringMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keys();
		for (i in it) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public inline function clear():Void {
		h = lua.Table.create();
	}
}
