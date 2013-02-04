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
class ObjectMap<K,V> {
   // TODO: Might need to add separate hash to keep track of references to keys
	private var __Internal : IntMap<V>;

	public function new() : Void {
		__Internal = new IntMap<V>();
	}

	public inline function set( key : K, value : V ) : Void {
		__Internal.set( untyped __global__.__hxcpp_obj_id(key), value );
	}

	public function get( key : K ) : V {
		return __Internal.get( untyped __global__.__hxcpp_obj_id(key) );
	}

	public inline function exists( key : K ) : Bool {
		return __Internal.exists( untyped __global__.__hxcpp_obj_id(key) );
	}

	public inline function remove( key : K ) : Bool {
		return __Internal.remove( untyped __global__.__hxcpp_obj_id(key) );
	}

	public function keys() : Iterator<K> {
		var a:Array<Int> = untyped __global__.__int_hash_keys(__Internal.h);
		var l = new Array<K>();
		for(id in a)
 			l.push(  untyped __global__.__hxcpp_id_obj(id) );
		return l.iterator();
	}

	public function iterator() : Iterator<V> {
		return __Internal.iterator();
	}

	private function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = __Internal.keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(__Internal.get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
