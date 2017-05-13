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
package python;

import python.internal.UBuiltins;
import python.Tuple;
import python.NativeIterator;
import python.Syntax;

@:native("dict")
extern class Dict<K,V> {
	function new ():Void;

	var length(get,never):Int;
	private inline function get_length():Int {
		return UBuiltins.len(this);
	}

	function get(key:K, ?def:V):V;

	inline function getSafe(key:K):V {
		return Syntax.arrayAccess(this, key);
	}

	inline function set(key:K, val:V):Void {
		Syntax.arraySet(this, key, val);
	}

	inline function remove(key:K):Void {
		Syntax.delete(python.Syntax.arrayAccess(this, key));
	}

	inline function hasKey(k:K):Bool {
		return Syntax.isIn(k, this);
	}

	function clear():Void;
	function copy():Dict<K,V>;
	function items():DictView<Tuple2<K,V>>;
	function keys():DictView<K>;
	function pop(key:K, ?def:V):V;
	function popitem():Tuple2<K,V>;
	function setdefault(key:K, ?def:V):V;
	function update(d:Dict<K,V>):Void;
	function values():DictView<V>;

	inline function iter():NativeIterator<K> {
		return UBuiltins.iter(this);
	}

	inline function iterator():Iterator<V> {
		return values().iter();
	}

	private function __iter__():NativeIterator<K>;
}

extern class DictView<T> {
	var length(get,never):Int;
	private inline function get_length():Int {
		return UBuiltins.len(this);
	}

	inline function iter():NativeIterator<T> {
		return UBuiltins.iter(this);
	}

	inline function iterator():Iterator<T> {
		return iter();
	}

	private function __iter__():NativeIterator<T>;
}
