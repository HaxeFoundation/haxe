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
	var dict:cs.system.collections.generic.Dictionary<String, T>;

	public function new():Void {
		dict = new cs.system.collections.generic.Dictionary<String, T>();
	}

	public function set(key:String, value:T):Void {
		cs.Syntax.code("{0}[{1}] = {2}", dict, key, value);
	}

	public function get(key:String):Null<T> {
		if (cs.Syntax.code("{0}.TryGetValue({1}, out var _hx_tmp)", dict, key)) {
			return cs.Syntax.code("_hx_tmp");
		}
		return null;
	}

	public function exists(key:String):Bool {
		return dict.ContainsKey(key);
	}

	public function remove(key:String):Bool {
		return dict.Remove(key);
	}

	public function keys():Iterator<String> {
		var keyArray = new Array<String>();
		var count:Int = cs.Syntax.code("{0}.Keys.Count", dict);
		var keysArr = new cs.NativeArray<String>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			keyArray.push(keysArr[i]);
			i++;
		}
		return keyArray.iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function iterator():Iterator<T> {
		var valueArray = new Array<T>();
		var count:Int = cs.Syntax.code("{0}.Values.Count", dict);
		var valuesArr = new cs.NativeArray<T>(count);
		cs.Syntax.code("{0}.Values.CopyTo({1}, 0)", dict, valuesArr);
		var i = 0;
		while (i < count) {
			valueArray.push(valuesArr[i]);
			i++;
		}
		return valueArray.iterator();
	}

	public function copy():StringMap<T> {
		var copied = new StringMap<T>();
		var count:Int = cs.Syntax.code("{0}.Keys.Count", dict);
		var keysArr = new cs.NativeArray<String>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			var key = keysArr[i];
			var val:T = cs.Syntax.code("{0}[{1}]", dict, key);
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
		var keysArr = new cs.NativeArray<String>(count);
		cs.Syntax.code("{0}.Keys.CopyTo({1}, 0)", dict, keysArr);
		var i = 0;
		while (i < count) {
			if (!first)
				s.add(", ");
			first = false;
			var key = keysArr[i];
			var val:T = cs.Syntax.code("{0}[{1}]", dict, key);
			s.add(key);
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
