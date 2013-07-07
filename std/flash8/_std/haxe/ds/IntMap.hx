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

@:coreApi class IntMap<T> implements Map.IMap<Int,T>{

	private var h : Dynamic;

	public function new() : Void {
		h = untyped __new__(_global["Object"]);
	}

	public function set( key : Int, value : T ) : Void {
		h[key] = value;
	}

	public function get( key : Int ) : Null<T> {
		return h[key];
	}

	public function exists( key : Int ) : Bool {
		return untyped h["hasOwnProperty"](key);
	}

	public function remove( key : Int ) : Bool {
		if( untyped !h["hasOwnProperty"](key) ) return false;
		untyped __delete__(h,key);
		return true;
	}

	public function keys() : Iterator<Int> {
		var l : Array<Int> = untyped __keys__(h);
		for( x in 0...l.length )
			l[x] = Std.int(l[x]);
		return l.iterator();
	}

	public function iterator() : Iterator<T> {
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return __this__.it[__unprotect__("hasNext")](); },
			next : function() { var i = __this__.it[__unprotect__("next")](); return __this__.ref[i]; }
		};
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
