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
class ObjectMap<A,B> implements haxe.Constraints.IMap<A,B> {

	static var count = 0;

	static inline function assignId(obj: { } ):Int {
		return untyped obj.__id__ = ++count;
	}

	static inline function getId(obj: { } ):Int {
		return untyped obj.__id__;
	}

	var h : Dynamic;
	var k : Dynamic;

	public inline function new() : Void {
		h = lua.Table.create();
		k = lua.Table.create();
	}

	public inline function set( key : A, value : B ) : Void untyped {
		h[key] = value;
		k[key] = true;
	}

	public inline function get( key : A ) : Null<B> untyped {
		return h[key];
	}

	public inline function exists( key : A ) : Bool untyped {
		return k[key] != null;
	}

	public function remove( key : A ) : Bool untyped {
		if ( k[key] == null) return false;
		k[key] = null;
		h[key] = null;
		return true;
	}

	public function keys() : Iterator<A> untyped {
		var cur = next(h,null);
		return {
			next : function() {
				var ret = cur;
				cur = untyped next(k, cur);
				return ret;
			},
			hasNext : function() return cur != null
		}
	}

	public function iterator() : Iterator<B> {
		var itr = keys();
		return untyped {
			hasNext : itr.hasNext, 
			next : function() return h[itr.next()]
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

