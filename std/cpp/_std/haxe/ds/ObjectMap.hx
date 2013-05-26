/*
 * Copyright (C)2005-2013 Haxe Foundation
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
class ObjectMap<K:{},V> implements Map.IMap<K,V> {
	private var __Internal : IntMap<V>;
	private var __KeyRefs : IntMap<K>;

	public function new() : Void {
		__Internal = new IntMap<V>();
		__KeyRefs = new IntMap<K>();
	}

	public function set( key : K, value : V ) : Void {
		var id = untyped __global__.__hxcpp_obj_id(key);
		__Internal.set( id, value );
		__KeyRefs.set( id, key );
	}

	public function get( key : K ) : Null<V> {
		return __Internal.get( untyped __global__.__hxcpp_obj_id(key) );
	}

	public inline function exists( key : K ) : Bool {
		return __Internal.exists( untyped __global__.__hxcpp_obj_id(key) );
	}

	public function remove( key : K ) : Bool {
		var id = untyped __global__.__hxcpp_obj_id(key);
		return __Internal.remove(id);
		return __KeyRefs.remove(id);
	}

	public function keys() : Iterator<K> {
		return __KeyRefs.iterator();
	}

	public function iterator() : Iterator<V> {
		return __Internal.iterator();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = __Internal.keys();
		for( i in it ) {
			s.add(Std.string(__KeyRefs.get(i)));
			s.add(" => ");
			s.add(Std.string(__Internal.get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
