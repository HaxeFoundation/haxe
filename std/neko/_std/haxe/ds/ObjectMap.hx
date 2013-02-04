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
abstract ObjectMap({})<K,V> {

	public function new() : Void {
		this = untyped __dollar__hnew(0);
	}

	public inline function set( key : K, value : V ) : Void {
		untyped __dollar__hset(this,key,value,null);
	}

	public function get( key : K ) : V {
		return untyped __dollar__hget(this,key,null);
	}

	public inline function exists( key : K ) : Bool {
		return untyped __dollar__hmem(this,key,null);
	}

	public inline function remove( key : K ) : Bool {
		return untyped __dollar__hremove(this,key,null);
	}

	public function keys() : Iterator<K> {
		var l = new List<K>();
		untyped __dollar__hiter(this,function(k,_) { l.push(k); });
		return l.iterator();
	}

	public function iterator() : Iterator<V> {
		var l = new List<V>();
		untyped __dollar__hiter(this,function(_,v) { l.push(v); });
		return l.iterator();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys(this);
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(this,i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
