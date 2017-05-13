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

@:coreApi class StringMap<T> implements haxe.Constraints.IMap<String,T> {

	private var h : Dynamic;
	private var rh : Dynamic;
	static var reserved = { };

	public function new() : Void {
		h = {};
	}

	inline function isReserved(key:String) : Bool {
		return untyped __in__(key,reserved);
	}

	public inline function set( key : String, value : T ) : Void {
		if( isReserved(key) )
			setReserved(key, value);
		else
			untyped h[key] = value;
	}

	public inline function get( key : String ) : Null<T> {
		if( isReserved(key) )
			return getReserved(key);
		return untyped h[key];
	}

	public inline function exists( key : String ) : Bool {
		if( isReserved(key) )
			return existsReserved(key);
		return untyped __in__(key,h);
	}

	function setReserved( key : String, value : T ) : Void {
		if( rh == null ) rh = {};
		untyped rh["$"+key] = value;
	}

	function getReserved( key : String ) : Null<T> {
		return rh == null ? null : untyped rh["$"+key];
	}

	function existsReserved( key : String ) : Bool {
		if( rh == null ) return false;
		return untyped __in__("$"+key,rh);
	}

	public function remove( key : String ) : Bool {
		if( isReserved(key) ) {
			key = "$" + key;
			if( rh == null || !untyped __in__(key,rh) ) return false;
			untyped __delete__(rh,key);
			return true;
		} else {
			if( !untyped __in__(key,h) )
				return false;
			untyped __delete__(h,key);
			return true;
		}
	}

	#if as3

	// unoptimized version

	public function keys() : Iterator<String> {
		var out : Array<String> = untyped __keys__(h);
		if( rh != null ) out = out.concat(untyped __hkeys__(rh));
		return out.iterator();
	}

	public function iterator() : Iterator<T> {
		return untyped {
			it : keys(),
			hasNext : function() { return __this__.it.hasNext(); },
			next : function() { return get(__this__.it.next()); }
		};
	}

	#else

	public inline function keys() : Iterator<String> {
		return new StringMapKeysIterator(h, rh);
	}

	public inline function iterator() : Iterator<T> {
		return new StringMapValuesIterator<T>(h, rh);
	}

	#end

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

#if !as3

// this version uses __has_next__/__forin__ special SWF opcodes for iteration with no allocation

@:allow(haxe.ds.StringMap)
private class StringMapKeysIterator {
	var h:Dynamic;
	var rh:Dynamic;
	var index : Int;
	var nextIndex : Int;
	var isReserved : Bool;

	inline function new(h:Dynamic, rh:Dynamic):Void {
		this.h = h;
		this.rh = rh;
		this.index = 0;
		isReserved = false;
		hasNext();
	}

	public inline function hasNext():Bool {
		var h = h, index = index; // tmp vars required for __has_next
		var n = untyped __has_next__(h, index);
		if( !n && rh != null ) {
			h = this.h = rh;
			index = this.index = 0;
			rh = null;
			isReserved = true;
			n = untyped __has_next__(h, index);
		}
		this.nextIndex = index; // store next index
		return n;
	}

	public inline function next():String {
		var r : String = untyped __forin__(h, nextIndex);
		index = nextIndex;
		if( isReserved ) r = r.substr(1);
		return r;
	}

}

@:allow(haxe.ds.StringMap)
private class StringMapValuesIterator<T> {
	var h:Dynamic;
	var rh:Dynamic;
	var index : Int;
	var nextIndex : Int;

	inline function new(h:Dynamic, rh:Dynamic):Void {
		this.h = h;
		this.rh = rh;
		this.index = 0;
		hasNext();
	}

	public inline function hasNext():Bool {
		var h = h, index = index; // tmp vars required for __has_next
		var n = untyped __has_next__(h, index);
		if( !n && rh != null ) {
			h = this.h = rh;
			index = this.index = 0;
			rh = null;
			n = untyped __has_next__(h, index);
		}
		this.nextIndex = index; // store next index
		return n;
	}

	public inline function next():T {
		var r = untyped __foreach__(h, nextIndex);
		index = nextIndex;
		return r;
	}

}
#end
