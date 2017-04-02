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

@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int,T> {

	private var h : flash.utils.Dictionary;

	public function new() : Void {
		h = new flash.utils.Dictionary();
	}

	public inline function set( key : Int, value : T ) : Void {
		untyped h[key] = value;
	}

	public inline function get( key : Int ) : Null<T> {
		return untyped h[key];
	}

	public inline function exists( key : Int ) : Bool {
		return untyped __in__(key,h);
	}

	public function remove( key : Int ) : Bool {
		if( !exists(key) ) return false;
		untyped __delete__(h,key);
		return true;
	}

	#if as3

	// unoptimized version
	
	public function keys() : Iterator<Int> {
		return untyped (__keys__(h)).iterator();
	}

	@:analyzer(ignore) public function iterator() : Iterator<T> {
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return __this__.it.hasNext(); },
			next : function() { var i = __this__.it.next(); return __this__.ref[i]; }
		};
	}

	#else

	public inline function keys() : Iterator<Int> {
		return new IntMapKeysIterator(h);
	}

	public inline function iterator() : Iterator<T> {
		return new IntMapValuesIterator<T>(h);
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

@:allow(haxe.ds.IntMap)
private class IntMapKeysIterator {
	var h:flash.utils.Dictionary;
	var index : Int;
	var nextIndex : Int;

	inline function new(h:flash.utils.Dictionary):Void {
		this.h = h;
		this.index = 0;
		hasNext();
	}

	public inline function hasNext():Bool {
		var h = h, index = index; // tmp vars required for __has_next
		var n = untyped __has_next__(h, index);
		this.nextIndex = index; // store next index
		return n;
	}

	public inline function next():Int {
		var r : Int = untyped __forin__(h, nextIndex);
		index = nextIndex;
		return r;
	}

}

@:allow(haxe.ds.IntMap)
private class IntMapValuesIterator<T> {
	var h:flash.utils.Dictionary;
	var index : Int;
	var nextIndex : Int;

	inline function new(h:flash.utils.Dictionary):Void {
		this.h = h;
		this.index = 0;
		hasNext();
	}

	public inline function hasNext():Bool {
		var h = h, index = index; // tmp vars required for __has_next
		var n = untyped __has_next__(h, index);
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
