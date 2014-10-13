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
	private var cachedKeys : Array<String>;

	public function new() : Void {
		h = {};
	}

	// reserved words that are not allowed in Dictionary include all non-static members of Dictionary
	static private inline var reservedWordCount = 13;
	static private var reservedWordIndices = {
			"constructor":0,
			"hasOwnProperty":1,
			"isPrototypeOf":2,
			"propertyIsEnumerable":3,
			"toLocaleString":4,
			"toString":5,
			"valueOf":6,
			"__definegetter__":7,
			"__definesetter__":8,
			"__lookupgetter__":9,
			"__lookupsetter__":10,
			"__proto__":11 // NOTE - you cannot find the correct value of this entry by [] indexing, so we have a special test in getReservedWordIndex
	};
	private inline function getReservedWordIndex(key:String):Int {
		if ( key == "__proto__" ) {
			return 11;
		} else {
			return untyped reservedWordIndices[key];
		}
	}
	private inline function reservedWordByIndex(index:Int):String {
		return switch(index) { 
			case 0: "constructor";
			case 1: "hasOwnProperty";
			case 2: "isPrototypeOf";
			case 3: "propertyIsEnumerable";
			case 4: "toLocaleString";
			case 5: "toString";
			case 6: "valueOf";
			case 7: "__definegetter__";
			case 8: "__definesetter__";
			case 9: "__lookupgetter__";
			case 10: "__lookupsetter__";
			case 11: "__proto__";
			default: null;
		}
	}
	
	public inline function set( key : String, value : T ) : Void {
		if( untyped reservedWordIndices[key] != null ) {
			setReserved(key, value);
		} else {
			untyped h[key] = value;
		}
	}
	private function setReserved( key : String, value : T ) : Void {
		if( rh == null ) {
			rh = {};
		}
		var i:Int = getReservedWordIndex(key);
		untyped rh[i] = value;		
	}

	public inline function get( key : String ) : Null<T> {		
		if( untyped reservedWordIndices[key] != null ) {
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
		var i:Int = getReservedWordIndex(key);
		var rv:Null<T> = untyped rh[i];
		return rv == null ? null : rv;
	}

	public inline function exists( key : String ) : Bool {
		if( untyped reservedWordIndices[key] != null ) {
			return existsReserved(key);
		} else {
			return untyped h.hasOwnProperty(key);
		}		
	}
	private function existsReserved( key : String ) : Bool {
		if( rh == null ) {
			return false;
		}
		var i:Int = getReservedWordIndex(key);
		return untyped rh.hasOwnProperty(i);
	}

	public function remove( key : String ) : Bool {
		if( untyped reservedWordIndices[key] != null ) {
			if( rh == null ) {
				return false;
			}
			var i:Int = getReservedWordIndex(key);
			if( !(untyped rh.hasOwnProperty(i)) ) {
				return false;
			} else {
				untyped __js__("delete")(rh[i]);
				return true;
			}
		} else {
			if( !(untyped h.hasOwnProperty(key)) ) {
				return false;
			} else {
				untyped __js__("delete")(h[key]);
				return true;
			}
		}
	}

	public function keys() : Iterator<String> {
		if ( cachedKeys == null ) {
			cachedKeys = new Array();
		}
		untyped {
			var i:Int = 0;
			__js__("for( var key in this.h ) {");
				cachedKeys[i] = key;
				i++;
			__js__("}");
			cachedKeys.length = i;
		}
		if( rh != null ) {
			var i:Int = reservedWordCount - 1;
			while(i >= 0) {
				if( untyped rh.hasOwnProperty(i) ) {
					cachedKeys.push(reservedWordByIndex(i));
				}
				i--;
			}
		}
		return cachedKeys.iterator();
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
