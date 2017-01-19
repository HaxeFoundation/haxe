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

@:coreApi
class ObjectMap <K:{ }, V> implements haxe.Constraints.IMap<K,V> {
	static function getId(key: { } ):String {
		return untyped __php__("spl_object_hash($key)");
	}

	@:analyzer(no_simplification)
	var h : ArrayAccess<V>;
	@:analyzer(no_simplification)
	var hk : ArrayAccess<K>;

	public function new():Void {
		h = untyped __call__('array');
		hk = untyped __call__('array');
	}

	public function set(key:K, value:V):Void untyped {
		var id = getId(key);
		untyped h[id] = value;
		untyped hk[id] = key;
	}

	public function get(key:K):Null<V> {
		var id = getId(key);
		if (untyped __call__("array_key_exists", id, h))
			return untyped h[id];
		else
			return null;
	}

	public function exists(key:K):Bool {
		return untyped __call__("array_key_exists", getId(key), h);
	}

	public function remove( key : K ) : Bool {
		var id = getId(key);
		if (untyped __call__("array_key_exists", id, h)) {
			untyped __call__("unset", h[id]);
			untyped __call__("unset", hk[id]);
			return true;
		} else
			return false;
	}

	public inline function keys() : Iterator<K> {
		return untyped __call__("new _hx_array_iterator", __call__("array_values", hk));
	}

	public inline function iterator() : Iterator<V> {
		return untyped __call__("new _hx_array_iterator", __call__("array_values", h));
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