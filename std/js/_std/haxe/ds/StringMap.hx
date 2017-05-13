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

private class StringMapIterator<T> {
	var map : StringMap<T>;
	var keys : Array<String>;
	var index : Int;
	var count : Int;
	public inline function new(map:StringMap<T>, keys:Array<String>) {
		this.map = map;
		this.keys = keys;
		this.index = 0;
		this.count = keys.length;
	}
	public inline function hasNext() {
		return index < count;
	}
	public inline function next() {
		return map.get(keys[index++]);
	}
}

@:coreApi class StringMap<T> implements haxe.Constraints.IMap<String,T> {

	private var h : Dynamic;
	private var rh : Dynamic;

	public inline function new() : Void {
		h = {};
	}

	inline function isReserved(key:String) : Bool {
		return untyped __js__("__map_reserved")[key] != null;
	}

	public inline function set( key : String, value : T ) : Void {
		if( isReserved(key) )
			setReserved(key, value);
		else
			h[cast key] = value;
	}

	public inline function get( key : String ) : Null<T> {
		if( isReserved(key) )
			return getReserved(key);
		return h[cast key];
	}

	public inline function exists( key : String ) : Bool {
		if( isReserved(key) )
			return existsReserved(key);
		return h.hasOwnProperty(key);
	}

	function setReserved( key : String, value : T ) : Void {
		if( rh == null ) rh = {};
		rh[cast "$"+key] = value;
	}

	function getReserved( key : String ) : Null<T> {
		return rh == null ? null : rh[cast "$"+key];
	}

	function existsReserved( key : String ) : Bool {
		if( rh == null ) return false;
		return untyped rh.hasOwnProperty("$"+key);
	}

	public function remove( key : String ) : Bool {
		if( isReserved(key) ) {
			key = "$" + key;
			if( rh == null || !rh.hasOwnProperty(key) ) return false;
			untyped __js__("delete")(rh[key]);
			return true;
		} else {
			if( !h.hasOwnProperty(key) )
				return false;
			untyped __js__("delete")(h[key]);
			return true;
		}
	}

	public function keys() : Iterator<String> {
		return arrayKeys().iterator();
	}
	
	function arrayKeys() : Array<String> {
		var out = [];
		untyped {
			__js__("for( var key in this.h ) {");
				if( h.hasOwnProperty(key) )
					out.push(key);
			__js__("}");
		}
		if( rh != null ) untyped {
			__js__("for( var key in this.rh ) {");
				if( key.charCodeAt(0) == "$".code )
					out.push(key.substr(1));
			__js__("}");
		}
		return out;
	}

	public inline function iterator() : Iterator<T> {
		return new StringMapIterator(this, arrayKeys());
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var keys = arrayKeys();
		for( i in 0...keys.length ) {
			var k = keys[i];
			s.add(k);
			s.add(" => ");
			s.add(Std.string(get(k)));
			if( i < keys.length-1 )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	static function __init__() : Void {
		untyped __js__("var __map_reserved = {};");
	}

}
