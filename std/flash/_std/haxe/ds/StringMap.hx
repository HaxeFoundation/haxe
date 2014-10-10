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
	
	private var h : flash.utils.Dictionary;

	public function new() : Void {
		h = new flash.utils.Dictionary();
	}

	// reserved words that are not allowed in Dictionary include all non-static members of Dictionary
	private static var reservedWordIndexers:Array<ReservedWordIndexer> = [new ReservedWordIndexer(0), new ReservedWordIndexer(1), new ReservedWordIndexer(2), new ReservedWordIndexer(3), new ReservedWordIndexer(4), new ReservedWordIndexer(5), new ReservedWordIndexer(6), new ReservedWordIndexer(7), new ReservedWordIndexer(8)];
	static private var reservedWordIndicesStatic = {
			"constructor":0,
			"hasOwnProperty":1,
			"isPrototypeOf":2,
			"propertyIsEnumerable":3,
			"setPropertyIsEnumerable":4,
			"toLocaleString":5,
			"toString":6,
			"valueOf":7,
			"toJSON":8
	};
	private var reservedWordIndices = reservedWordIndicesStatic;
	private inline function reservedWordByIndex(index:Int):String {
		return switch(index) { 
			case 0: "constructor";
			case 1: "hasOwnProperty";
			case 2: "isPrototypeOf";
			case 3: "propertyIsEnumerable";
			case 4: "setPropertyIsEnumerable";
			case 5: "toLocaleString";
			case 6: "toString";
			case 7: "valueOf";
			case 8: "toJSON";
			default: null;
		}
	}
	public inline function set( key : String, value : T ) : Void {
		if ( untyped __in__(key, reservedWordIndices) ) {
			setReserved(key, value);
		} else {
			untyped h[key] = value;
		}
	}
	private function setReserved( key : String, value : T ) : Void {
		var i:Int = untyped reservedWordIndices[key];
		untyped h[reservedWordIndexers[i]] = value;		
	}

	public inline function get( key : String ) : Null<T> {		
		if ( untyped __in__(key, reservedWordIndices) ) {
			return getReserved(key);
		} else {
			var rv:Null<T> = untyped h[key];
			return rv == null ? null : rv;
		}
	}
	private function getReserved( key : String ) : Null<T> {
		var i:Int = untyped reservedWordIndices[key];
		return untyped h[reservedWordIndexers[i]];
	}

	public inline function exists( key : String ) : Bool {
		if ( untyped __in__(key, reservedWordIndices) ) {
			return existsReserved(key);
		} else {
			return untyped __in__(key, h);
		}		
	}
	private function existsReserved( key : String ) : Bool {
		var i:Int = untyped reservedWordIndices[key];
		return untyped __in__(reservedWordIndexers[i], h);
	}

	public inline function remove( key : String ) : Bool {
		if ( untyped __in__(key, reservedWordIndices) ) {
			var i:Int = untyped reservedWordIndices[key];
			if ( !(untyped __in__(reservedWordIndexers[i], h)) ) {
				return false;
			} else {
				untyped __delete__(h, reservedWordIndexers[i]);
				return true;
			}
		} else {
			if ( !(untyped __in__(key, h)) ) {
				return false;
			} else {
				untyped __delete__(h, key);
				return true;
			}
		}
	}

	public function keys() : Iterator<String> {
		var rv = untyped (__keys__(h));
		var len:Int = untyped rv.length;
		var i:Int = 0;
		while (i < len) {
			if ( Std.is(untyped rv[i], ReservedWordIndexer) ) {
				var indexer:ReservedWordIndexer = untyped rv[i];
				untyped rv[i] = reservedWordByIndex(indexer.i);
			}
			i++;
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

@:allow(haxe.ds.StringMap)
class ReservedWordIndexer {
	private var i:Int;
	private function new(i:Int):Void {
		this.i = i;
	}
}
