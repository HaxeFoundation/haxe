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

class StringMap<T> implements haxe.Constraints.IMap<String,T> {

	private var k : Dynamic; // Is item exists
	private var v : Dynamic; // Values table

	public inline function new() : Void {
		// these need to be plain anonymous tables,
		// so that we can set arbitrary string values.
		v = untyped __lua__("{}");
		k = untyped __lua__("{}");
	}

	public inline function set( key : String, value : T ) : Void untyped {
		untyped __lua__("{0}[{1}] = {2}", v, key, value);
		untyped __lua__("{0}[{1}] = true", k, key);
	}

	public inline function get( key : String ) : Null<T> untyped {
		return untyped __lua__("{0}[{1}]", v, key);
	}

	public inline function exists( key : String ) : Bool untyped {
		return untyped __lua__("({0}[{1}] or false)", k, key);
	}

	public function remove( key : String ) : Bool untyped {
		if (untyped __lua__("not {0}[{1}]", k, key)) return false;
		untyped __lua__("{0}[{1}] = nil", v, key);
		untyped __lua__("{0}[{1}] = nil", k, key);
		return true;
	}

	public function keys() : Iterator<String> {
		var cur : Array<String> = [];
		untyped __lua__("for _k,_v in pairs({1}) do
			if(_v)then {0}:push(_k) end
		end", cur, k);
		return {
			next : function() {
				var ret = cur.pop();
				return ret;
			},
			hasNext : function() return cur.length > 0
		}
	}

	public function iterator() : Iterator<T> {
		var it = keys();
		return  {
			hasNext : function() return it.hasNext(),
			next : function() return untyped __lua__("{0}[{1}]", v, it.next())
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

