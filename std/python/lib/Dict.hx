/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package python.lib;

import python.internal.UBuiltins;
import python.lib.Tuple;
import python.NativeIterator;
import python.Syntax;


extern class DictView<T> {
	private function __iter__():NativeIterator<T>;
	public inline function iter ():NativeIterator<T>
	{
		return UBuiltins.iter(this);
	}
	public inline function length ():Int
	{
		return UBuiltins.len(this);
	}

	public inline function iterator ():Iterator<T>
	{
		return iter();
	}
}

@:pythonImport("builtins", "dict")
extern class Dict <K, V>
{
	public function new ():Void;

	public inline function length ():Int
	{
		return UBuiltins.len(this);
	}

	public inline function hasKey (k:K):Bool {
		return DictImpl.hasKey(this,k);
	}

	public function clear ():Void;
	public function copy ():Dict<K,V>;
	public function get (key:K, def:V):V;

	public function update (d:Dict<K,V>):Void;

	public function keys ():DictView<K>;
	public function values ():DictView<V>;
	public function items ():DictView<Tup2<K,V>>;


	public inline function set (key:K, val:V):Void {
		DictImpl.set(this, key, val);
	}

	public inline function remove (key:K):Void
	{
		DictImpl.remove(this, key);
	}

	public inline function iterator ():Iterator<V>
	{
		return values().iter();
	}
	private function __iter__():NativeIterator<K>;
}

class DictImpl {

	public static inline function hasKey <X>(d:Dict<X, Dynamic>, key:X) {
		return Syntax.isIn(key, d);
	}

	public static inline function remove <X>(d:Dict<X, Dynamic>, key:X) {
		Syntax.delete(python.Syntax.arrayAccess(d, key));
	}

	public static inline function set <K,V>(d:Dict<K, V>, key:K, val:V) {
		Syntax.arraySet(d, key, val);
	}
}