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

@:coreApi class StringMap<T> implements haxe.Constraints.IMap<String,T> {
	
	private var h : Dynamic;
	private var rh : Dynamic;

	public function new() : Void {
		h = {};
	}

	// reserved words that are not allowed in Dictionary include all non-static members of Dictionary
	@:allow(haxe.ds.StringMapKeysIterator)
	@:allow(haxe.ds.StringMapValuesIterator)
	static private inline var reservedWordCount = 8;
	static private var reservedWordIndicesStatic = {
			"constructor":0,
			"hasOwnProperty":1,
			"isPrototypeOf":2,
			"propertyIsEnumerable":3,
			"setPropertyIsEnumerable":4,
			"toLocaleString":5,
			"toString":6,
			"valueOf":7
	};
	private var reservedWordIndices = reservedWordIndicesStatic;
	@:allow(haxe.ds.StringMapKeysIterator)
	@:allow(haxe.ds.StringMapValuesIterator)
	static private inline function reservedWordByIndex(index:Int):String {
		return switch(index) { 
			case 0: "constructor";
			case 1: "hasOwnProperty";
			case 2: "isPrototypeOf";
			case 3: "propertyIsEnumerable";
			case 4: "setPropertyIsEnumerable";
			case 5: "toLocaleString";
			case 6: "toString";
			case 7: "valueOf";
			default: null;
		}
	}
	
	public inline function set( key : String, value : T ) : Void {
		if( untyped __in__(key, reservedWordIndices) ) {
			setReserved(key, value);
		} else {
			untyped h[key] = value;
		}
	}
	private function setReserved( key : String, value : T ) : Void {
		if( rh == null ) {
			rh = {};
		}
		var i:Int = untyped reservedWordIndices[key];
		untyped rh[i] = value;		
	}

	public inline function get( key : String ) : Null<T> {		
		if( untyped __in__(key, reservedWordIndices) ) {
			return getReserved(key);
		} else {
			var rv:Null<T> = untyped h[key];
			return rv == null ? null : rv;
		}
	}
	private function getReserved( key : String ) : Null<T> {
		if( rh == null ) {
			return null;
		}
		var i:Int = untyped reservedWordIndices[key];
		var rv:Null<T> = untyped rh[i];
		return rv == null ? null : rv;
	}

	public inline function exists( key : String ) : Bool {
		if( untyped __in__(key, reservedWordIndices) ) {
			return existsReserved(key);
		} else {
			return untyped __in__(key, h);
		}		
	}
	private function existsReserved( key : String ) : Bool {
		if( rh == null ) {
			return false;
		}
		var i:Int = untyped reservedWordIndices[key];
		return untyped __in__(i, rh);
	}

	public function remove( key : String ) : Bool {
		if( untyped __in__(key, reservedWordIndices) ) {
			if( rh == null ) {
				return false;
			}
			var i:Int = untyped reservedWordIndices[key];
			if( !(untyped __in__(i, rh)) ) {
				return false;
			} else {
				untyped __delete__(rh, i);
				return true;
			}
		} else {
			if( !(untyped __in__(key, h)) ) {
				return false;
			} else {
				untyped __delete__(h, key);
				return true;
			}
		}
	}
	
	#if flash9
	public inline function keys() : Iterator<String> {
		return new StringMapKeysIterator(h, rh, 0);
	}
	
	public inline function iterator() : Iterator<T> {
		return new StringMapValuesIterator(h, rh, 0);
	}
	#else
	public function keys() : Iterator<String> {
		var rv = untyped (__keys__(h));
		if( rh != null ) {
			var i:Int = reservedWordCount - 1;
			while(i >= 0) {
				if( untyped __in__(i, rh) ) {
					rv.push(reservedWordByIndex(i));
				}
				i--;
			}
		}
		return rv.iterator();
	}
	
	public function iterator() : Iterator<T> {
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return __this__.it.hasNext(); },
			next : function() { var i : Dynamic = __this__.it.next(); return __this__.ref[i]; }
		};
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

private class StringMapKeysIterator {
	var collection:Dynamic;
	var rh:Dynamic;
	var index:Int;

	@:allow(haxe.ds.StringMap)
	inline function new(collection:Dynamic, rh:Dynamic, index:Int):Void {
		this.collection = collection;
		this.rh = rh;
		this.index = index;
	}

	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		if ( !result ) {
			if( rh != null && rh != collection ) {
				c = rh;
				i = 0;
				result = untyped __has_next__(c, i);
			}
		}
		collection = c;
		index = i;
		return result;
	}

	public inline function next():String {
		var result;
		var i = index;
		if( collection == rh ) {
			result = StringMap.reservedWordByIndex(untyped __forin__(collection, i));
		} else {
			result = untyped __forin__(collection, i);
		}
		index = i;
		return result;
	}
}

private class StringMapValuesIterator<T> {
	var collection:Dynamic;
	var rh:Dynamic;
	var index:Int;

	@:allow(haxe.ds.StringMap)
	inline function new(collection:Dynamic, rh:Dynamic, index:Int):Void {
		this.collection = collection;
		this.rh = rh;
		this.index = index;
	}
	public inline function iterator():Iterator<T> {
		return this;
	}

	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		if ( !result ) {
			if( rh != null && rh != collection ) {
				c = rh;
				i = 0;
				result = untyped __has_next__(c, i);
			}
		}
		collection = c;
		index = i;
		return result;
	}

	public inline function next():T {
		var i = index;
		var result = untyped __foreach__(collection, i);
		index = i;
		return result;
	}
}

