/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	Hashtable over a set of elements, using [Int] as keys.
	On Flash and Javascript, the underlying structure is an Object.
**/
class IntHash<T> {

	/**
		Creates a new empty hashtable.
	**/
	public function new() : Void {
		#if flash
		h = untyped __new__(Object);
		#else neko
		h = untyped __dollar__hnew(0);
		#else js
		h = untyped __js__("{}");
		untyped if( h.__proto__ != null ) {
			h.__proto__ = null;
			__js__("delete")(h.__proto__);
		}
		#else error
		#end
	}

	/**
		Set a value for the given key.
	**/
	public function set( key : Int, value : T ) : Void {
		#if flash
		untyped h[key] = value;
		#else js
		untyped h[key] = value;
		#else neko
		untyped __dollar__hset(h,key,value,null);
		#else error
		#end
	}

	/**
		Get a value for the given key.
	**/
	public function get( key : Int ) : T {
		#if flash
		return untyped h[key];
		#else js
		return untyped h[key];
		#else neko
		return untyped __dollar__hget(h,key,null);
		#else error
		#end
	}

	/**
		Tells if a value exists for the given key.
		In particular, it's useful to tells if a key has
		a [null] value versus no value.
	**/
	public function exists( key : Int ) : Bool {
		#if flash
		return untyped h.hasOwnProperty(key);
		#else js
		return untyped h[key] != null;
		#else neko
		return untyped __dollar__hmem(h,key,null);
		#else error
		#end
	}

	/**
		Removes a hashtable entry. Returns [true] if
		there was such entry.
	**/
	public function remove( key : Int ) : Bool {
		#if flash
		if( untyped !h.hasOwnProperty(key) ) return false;
		untyped __delete__(h,key);
		return true;
		#else js
		if( untyped h[key] == null ) return false;
		untyped  __js__("delete")(h[key]);
		return true;
		#else neko
		return untyped __dollar__hremove(h,key,null);
		#else error
		#end
	}

	/**
		Returns an iterator of all keys in the hashtable.
	**/
	public function keys() : Iterator<Int> {
		#if flash
		var l : Array<Int> = untyped __keys__(h);
		for( x in 0...l.length )
			l[x] = Std.int(l[x]);
		return l.iterator();
		#else js
		var a = new Array();
		untyped __js__("
			for( x in this.h )
				a.push(x);
		");
		return a.iterator();
		#else neko
		var l = new List<Int>();
		untyped __dollar__hiter(h,function(k,_) { l.push(k); });
		return l.iterator();
		#else error
		#end
	}

	/**
		Returns an iterator of all values in the hashtable.
	**/
	public function iterator() : Iterator<T> {
		#if flash
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return this.it[__unprotect__("hasNext")](); },
			next : function() { var i = this.it[__unprotect__("next")](); return this.ref[i]; }
		};
		#else js
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref[i]; }
		};
		#else neko
		var l = new List<T>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
		#else error
		#end
	}

	/**
		Returns an displayable representation of the hashtable content.
	**/

	public function toString() {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(get(i));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	private var h : Dynamic;

}
