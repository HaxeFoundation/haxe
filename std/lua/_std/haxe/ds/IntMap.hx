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
import lua.Lua;

class IntMap<T> implements haxe.Constraints.IMap<Int,T> {

	private var h : Dynamic;

	public inline function new() : Void {
		h = {};
	}

	public inline function set( key : Int, value : T ) : Void {
		 h[key] = value;
	}

	public inline function get( key : Int ) : Null<T> {
		return h[key];
	}

	public inline function exists( key : Int ) : Bool {
		return Reflect.hasField(h,cast key);
	}

	public function remove( key : Int ) : Bool {
		if (!Reflect.hasField(h,cast key)) return false;
		Reflect.deleteField(h, cast key);
		return true;
	}

	public function keys() : Iterator<Int> {
		var cur = Reflect.fields(h).iterator();
		return {
			next : function() {
				var ret = cur.next();
				return cast ret;
			},
			hasNext : function() return cur.hasNext()
		}
	}

	public function iterator() : Iterator<T> {
		var it = keys();
		return untyped {
			hasNext : function() return it.hasNext(),
			next : function() return h[it.next()]
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

