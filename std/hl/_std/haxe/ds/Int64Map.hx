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

#if (hl_ver >= version("1.13.0") && !hl_legacy32)

@:coreApi
class Int64Map<T> implements haxe.Constraints.IMap<haxe.Int64, T> {
	var h:hl.types.Int64Map;

	public function new():Void {
		h = new hl.types.Int64Map();
	}

	public function set(key:haxe.Int64, value:T):Void {
		@:privateAccess h.set(key, value);
	}

	public function get(key:haxe.Int64):Null<T> {
		return @:privateAccess h.get(key);
	}

	public function exists(key:haxe.Int64):Bool {
		return @:privateAccess h.exists(key);
	}

	public function remove(key:haxe.Int64):Bool {
		return @:privateAccess h.remove(key);
	}

	public function keys():Iterator<haxe.Int64> {
		return new hl.NativeArray.NativeArrayIterator<haxe.Int64>(h.keysArray());
	}

	public function iterator():Iterator<T> {
		return h.iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<haxe.Int64, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():Int64Map<T> {
		var copied = new Int64Map();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		var keys = h.keysArray();
		var values = h.valuesArray();
		s.addChar("[".code);
		for (i in 0...keys.length) {
			if (i > 0)
				s.add(", ");
			s.add(keys[i]);
			s.add(" => ");
			s.add(values[i]);
		}
		s.addChar("]".code);
		return s.toString();
	}

	public function clear():Void {
		@:privateAccess h.clear();
	}

	public function size():Int {
		return h.size();
	}
}

#else

// Same as haxe.ds.Int64Map
class Int64Map<T> implements haxe.Constraints.IMap<haxe.Int64, T> {
	var m : Map<String,T>;

	public function new():Void {
		m = new Map();
	}

	public function set(key:Int64, value:T):Void {
		m.set(key.toString(),value);
	}

	public function get(key:Int64):Null<T> {
		return m.get(key.toString());
	}

	public function exists(key:Int64):Bool {
		return m.exists(key.toString());
	}

	public function remove(key:Int64):Bool {
		return m.remove(key.toString());
	}

	public function keys():Iterator<Int64> {
		var it = m.keys();
		return {
			hasNext : () -> it.hasNext(),
			next: () -> {
				return haxe.Int64.parseString(it.next());
			}
		};
	}

	public function iterator():Iterator<T> {
		return m.iterator();
	}

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

	public function copy():Int64Map<T> {
		var v = new Int64Map();
		v.m = m.copy();
		return v;
	}

	public function toString():String {
		return m.toString();
	}

	public function clear():Void {
		m.clear();
	}

	public function size():Int {
		return m.size();
	}
}

#end
