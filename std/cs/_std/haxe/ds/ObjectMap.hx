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
class ObjectMap<K:{}, V> implements haxe.Constraints.IMap<K, V> {
	var dict:cs.system.collections.generic.Dictionary<K, V>;

	public function new():Void {
		dict = new cs.system.collections.generic.Dictionary<K, V>();
	}

	public function set(key:K, value:V):Void {
		cs.Syntax.code("{0}[{1}] = {2}", dict, key, value);
	}

	public function get(key:K):Null<V> {
		if (cs.Syntax.code("{0}.TryGetValue({1}, out var _hx_tmp)", dict, key)) {
			return cs.Syntax.code("_hx_tmp");
		}
		return null;
	}

	public function exists(key:K):Bool {
		return dict.ContainsKey(key);
	}

	public function remove(key:K):Bool {
		return dict.Remove(key);
	}

	public function keys():Iterator<K> {
		var keyArray = new Array<K>();
		var count:Int = cs.Syntax.code("{0}.Keys.Count", dict);
		var keysArr = new cs.NativeArray<K>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			keyArray.push(keysArr[i]);
			i++;
		}
		return keyArray.iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<K, V> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function iterator():Iterator<V> {
		var valueArray = new Array<V>();
		var count:Int = cs.Syntax.code("{0}.Values.Count", dict);
		var valuesArr = new cs.NativeArray<V>(count);
		cs.Syntax.code("{0}.Values.CopyTo({1}, 0)", dict, valuesArr);
		var i = 0;
		while (i < count) {
			valueArray.push(valuesArr[i]);
			i++;
		}
		return valueArray.iterator();
	}

	public function copy():ObjectMap<K, V> {
		var copied = new ObjectMap<K, V>();
		var count:Int = cs.Syntax.code("{0}.Keys.Count", dict);
		var keysArr = new cs.NativeArray<K>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			var key = keysArr[i];
			var val:V = cs.Syntax.code("{0}[{1}]", dict, key);
			cs.Syntax.code("{0}[{1}] = {2}", copied.dict, key, val);
			i++;
		}
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var first = true;
		var count:Int = cs.Syntax.code("{0}.Keys.Count", dict);
		var keysArr = new cs.NativeArray<K>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			if (!first)
				s.add(", ");
			first = false;
			var key = keysArr[i];
			var val:V = cs.Syntax.code("{0}[{1}]", dict, key);
			s.add(Std.string(key));
			s.add(" => ");
			s.add(Std.string(val));
			i++;
		}
		s.add("]");
		return s.toString();
	}

	public function clear():Void {
		dict.Clear();
	}

	public inline function size():Int {
		return dict.Count;
	}
}
