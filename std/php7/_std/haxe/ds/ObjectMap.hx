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

package haxe.ds;

import php.*;

@:coreApi
class ObjectMap <K:{ }, V> implements haxe.Constraints.IMap<K,V> {
	var _keys:NativeAssocArray<K>;
	var _values:NativeAssocArray<V>;

	public function new():Void {
		_keys = new NativeAssocArray();
		_values = new NativeAssocArray();
	}

	public function set(key:K, value:V):Void {
		var id = Global.spl_object_hash(key);
		_keys[id] = key;
		_values[id] = value;
	}

	public function get(key:K):Null<V> {
		var id = Global.spl_object_hash(key);
		return Global.isset(_values[id]) ? _values[id] : null;
	}

	public function exists(key:K):Bool {
		return Global.array_key_exists(Global.spl_object_hash(key), _values);
	}

	public function remove( key : K ) : Bool {
		var id = Global.spl_object_hash(key);
		if (Global.array_key_exists(id, _values)) {
			Global.unset(_keys[id], _values[id]);
			return true;
		} else {
			return false;
		}
	}

	public inline function keys() : Iterator<K> {
		return _keys.iterator();
	}

	public inline function iterator() : Iterator<V> {
		return _values.iterator();
	}

	public function toString() : String {
		var s = "{";
		var it = keys();
		for( i in it ) {
			s += Std.string(i);
			s += " => ";
			s += Std.string(get(i));
			if( it.hasNext() )
				s += ", ";
		}
		return s + "}";
	}
}