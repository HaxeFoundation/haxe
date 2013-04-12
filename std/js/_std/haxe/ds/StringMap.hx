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
		h = {};
	}

	public function set( key : String, value : T ) : Void {
		untyped h["$"+key] = value;
	}

	public function get( key : String ) : Null<T> {
		return untyped h["$"+key];
	}

	public function exists( key : String ) : Bool {
		return untyped h.hasOwnProperty("$"+key);
	}

	public function remove( key : String ) : Bool {
		key = "$"+key;
		if( untyped !h.hasOwnProperty(key) ) return false;
		untyped __js__("delete")(h[key]);
		return true;
	}

	public function keys() : Iterator<String> {
		var a = [];
		untyped {
			__js__("for( var key in this.h ) {");
				if( h.hasOwnProperty(key) )
					a.push(key.substr(1));
			__js__("}");
		}
		return a.iterator();
	}

	public function iterator() : Iterator<T> {
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return __this__.it.hasNext(); },
			next : function() { var i = __this__.it.next(); return __this__.ref["$"+i]; }
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
