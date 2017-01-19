/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of h software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and h permission notice shall be included in
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
class ObjectMap<K:{},V> implements haxe.Constraints.IMap<K,V> {

	static var count = 0;

	static inline function assignId(obj: { } ):Int {
		var newId = count++;
		untyped obj.__id__ = newId;
		return newId;
	}

	static inline function getId(obj: { } ):Int {
		return untyped obj.__id__;
	}

	var h : { };
	var k : { };

	public function new() : Void {
		h = untyped __dollar__hnew(0);
		k = untyped __dollar__hnew(0);
	}

	public inline function set( key : K, value : V ) : Void untyped {
		var id = key.__id__ != null ? key.__id__ : assignId(key);
		untyped __dollar__hset(h,id,value,null);
		untyped __dollar__hset(k,id,key,null);
	}

	public function get( key : K ) : Null<V> {
		return untyped __dollar__hget(h,getId(key),null);
	}

	public inline function exists( key : K ) : Bool {
		return untyped __dollar__hmem(h,getId(key),null);
	}

	public function remove( key : K ) : Bool {
		var id = getId(key);
		untyped __dollar__hremove(h,id,null);
		return untyped __dollar__hremove(k,id,null);
	}

	public function keys() : Iterator<K> {
		var l = new List<K>();
		untyped __dollar__hiter(k,function(_,v) { l.push(v); });
		return l.iterator();
	}

	public function iterator() : Iterator<V> {
		var l = new List<V>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(Std.string(i));
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
