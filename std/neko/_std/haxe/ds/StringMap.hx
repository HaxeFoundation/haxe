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

@:coreApi class StringMap<T> implements Map.IMap<String,T> {

	private var h : Dynamic;

	public function new() : Void {
		h = untyped __dollar__hnew(0);
	}

	public inline function set( key : String, value : T ) : Void {
		untyped __dollar__hset(h,key.__s,value,null);
	}

	public inline function get( key : String ) : Null<T> {
		return untyped __dollar__hget(h,key.__s,null);
	}

	public inline function exists( key : String ) : Bool {
		return untyped __dollar__hmem(h,key.__s,null);
	}

	public inline function remove( key : String ) : Bool {
		return untyped __dollar__hremove(h,key.__s,null);
	}

	public function keys() : Iterator<String> {
		var l = new List<String>();
		untyped __dollar__hiter(h,function(k,_) { l.push(new String(k)); });
		return l.iterator();
	}

	public function iterator() : Iterator<T> {
		var l = new List<T>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
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
