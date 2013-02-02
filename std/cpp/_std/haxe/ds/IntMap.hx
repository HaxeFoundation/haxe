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
package haxe.ds;

@:coreApi class IntMap<T> {

	private var h : Dynamic;

	public function new() : Void {
		h = untyped __global__.__int_hash_create();
	}

	public function set( key : Int, value : T ) : Void {
		untyped __global__.__int_hash_set(h,key,value);
	}

	public function get( key : Int ) : Null<T> {
		return untyped __global__.__int_hash_get(h,key);
	}

	public function exists( key : Int ) : Bool {
		return untyped __global__.__int_hash_exists(h,key);
	}

	public function remove( key : Int ) : Bool {
		return untyped __global__.__int_hash_remove(h,key);
	}

	public function keys() : Iterator<Int> {
		var a:Array<Int> = untyped __global__.__int_hash_keys(h);
		return a.iterator();
	}

	public function iterator() : Iterator<T> {
		var a:Array<Dynamic> = untyped __global__.__int_hash_values(h);
		return a.iterator();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

}
