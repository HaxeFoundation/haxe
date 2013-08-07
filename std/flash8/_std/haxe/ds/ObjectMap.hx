/*
 * Copyright (C)2005-2013 Haxe Foundation
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
class ObjectMap <K:{ }, V> implements Map.IMap<K,V> {
	
	static var count = 0;
	
	static inline function assignId(obj: { } ):Int {
		return untyped obj.__id__ = ++count;
	}
	
	static inline function getId(obj: { } ):Int {
		return untyped obj.__id__;
	}
	
	var h: { };
	
	public function new():Void {
		h = untyped __new__(_global["Object"]);
		untyped h.__keys__ = untyped __new__(_global["Object"]);
	}
	
	public function set(key:K, value:V):Void untyped {
		var id = "$" + (key.__id__ != null ? key.__id__ : assignId(key));
		h[id] = value;
		h.__keys__[id] = key;
	}
	
	public inline function get(key:K):Null<V> {
		return untyped h["$" +getId(key)];
	}
	
	public inline function exists(key:K):Bool {
		return untyped h["hasOwnProperty"]("$" +getId(key));
	}
	
	public function remove( key : K ) : Bool {
		var key = "$" + getId(key);
		if( untyped !h["hasOwnProperty"](key) ) return false;
		untyped __delete__(h,key);
		untyped __delete__(h.__keys__,key);
		return true;
	}
	
	public function keys() : Iterator<K> {
		var a = [];
		untyped {
			var keys:Iterator<String> = __hkeys__(h.__keys__)["iterator"]();
			for (key in keys) {
				if( h.hasOwnProperty("$" +key) )
					a.push(h.__keys__["$" +key]);
			}
		}
		return a.iterator();
	}
	
	public function iterator() : Iterator<V> {
		return untyped {
			ref : h,
			it : __hkeys__(h.__keys__)["iterator"](),
			hasNext : function() { return __this__.it[__unprotect__("hasNext")](); },
			next : function() { var i = __this__.it[__unprotect__("next")](); return __this__.ref["$" +i]; }
		};
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