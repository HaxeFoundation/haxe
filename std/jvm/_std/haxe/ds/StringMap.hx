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

@:coreApi
class StringMap<T> implements haxe.Constraints.IMap<String, T> {
	var hashMap:java.util.HashMap<String, T>;

	@:overload
	public function new():Void {
		hashMap = new java.util.HashMap();
	}

	@:overload
	function new(hashMap:java.util.HashMap<String, T>):Void {
		this.hashMap = hashMap;
	}

	public function set(key:String, value:T):Void {
		hashMap.put(key, value);
	}

	public function get(key:String):Null<T> {
		return hashMap.get(key);
	}

	public function exists(key:String):Bool {
		return hashMap.containsKey(key);
	}

	public function remove(key:String):Bool {
		var has = exists(key);
		hashMap.remove(key);
		return has;
	}

	public inline function keys():Iterator<String> {
		return hashMap.keySet().iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public inline function iterator():Iterator<T> {
		return hashMap.values().iterator();
	}

	public function copy():StringMap<T> {
		return new StringMap(hashMap.clone());
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

	public function clear():Void {
		hashMap.clear();
	}
}
