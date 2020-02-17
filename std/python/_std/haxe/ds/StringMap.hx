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

import python.Syntax;
import python.Dict;

class StringMap<T> implements haxe.Constraints.IMap<String, T> {
	private var h:Dict<String, T>;

	public function new():Void {
		h = new Dict();
	}

	public inline function set(key:String, value:T):Void {
		h.set(key, value);
	}

	public inline function get(key:String):Null<T> {
		return h.get(key, null);
	}

	public inline function exists(key:String):Bool {
		return h.hasKey(key);
	}

	public function remove(key:String):Bool {
		var has = h.hasKey(key);
		if (has)
			h.remove(key);
		return has;
	}

	public function keys():Iterator<String> {
		return h.keys().iter();
	}

	public function iterator():Iterator<T> {
		return h.values().iter();
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

		s.add("{");
		var it = keys();
		for (i in it) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	public inline function clear():Void {
		h.clear();
	}
}
