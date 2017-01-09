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

@:coreApi class StringMap<T> implements php.IteratorAggregate<T> implements haxe.Constraints.IMap<String,T> {
	@:analyzer(no_simplification)
	private var h : ArrayAccess<T>;

	public function new() : Void {
		h = untyped __call__('array');
	}

	public function set( key : String, value : T ) : Void {
		untyped h[key] = value;
	}

	public function get( key : String ) : Null<T> {
		if (untyped __call__("array_key_exists", key, h))
			return untyped h[key];
		else
			return null;
	}

	public function exists( key : String ) : Bool {
		return untyped __call__("array_key_exists", key, h);
	}

	public function remove( key : String ) : Bool {
		if (untyped __call__("array_key_exists", key, h)) {
			untyped __call__("unset", h[key]);
			return true;
		} else
			return false;
	}

	public function keys() : Iterator<String> {
		return untyped __call__("new _hx_array_iterator", __call__("array_map", "strval", __call__("array_keys", h)));
	}

	public function iterator() : Iterator<T> {
		return untyped __call__("new _hx_array_iterator", __call__("array_values", h));
	}

	public function toString() : String {
		var s = "{";
		var it = keys();
		for( i in it ) {
			s += i;
			s += " => ";
			s += Std.string(get(i));
			if( it.hasNext() )
				s += ", ";
		}
		return s + "}";
	}

	/**
		Implement IteratorAggregate for native php iteration
	**/
	#if php
	function getIterator() : Iterator<T> {
		return iterator();
	}
	#end
}
