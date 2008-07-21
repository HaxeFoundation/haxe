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
	Hashtable over a set of elements, using [String] as keys.
	Other kind of keys are not possible on all platforms since they
	can't always be implemented efficiently.
**/
class Hash<T> {

	private var h : #if flash9 flash.utils.Dictionary #elseif php ArrayAccess<T> #else Dynamic #end;

	/**
		Creates a new empty hashtable.
	**/
	public function new() : Void {
		#if flash9
		h = new flash.utils.Dictionary();
		#elseif flash
		h = untyped __new__(_global["Object"]);
		#elseif neko
		h = untyped __dollar__hnew(0);
		#elseif js
		untyped {
			h = __js__("{}");
			if( h.__proto__ != null ) {
				h.__proto__ = null;
				__js__("delete")(h.__proto__);
			}
		}
		#elseif php
		h = untyped __call__('array');
		#end
	}

	/**
		Set a value for the given key.
	**/
	public function set( key : String, value : T ) : Void {
		#if flash
		untyped h["$"+key] = value;
		#elseif js
		untyped h["$"+key] = value;
		#elseif neko
		untyped __dollar__hset(h,key.__s,value,null);
		#elseif php
		untyped __php__("$this->h[$key] = $value");
		#end
	}

	/**
		Get a value for the given key.
	**/
	public function get( key : String ) : Null<T> {
		#if flash
		return untyped h["$"+key];
		#elseif js
		return untyped h["$"+key];
		#elseif neko
		return untyped __dollar__hget(h,key.__s,null);
		#elseif php
		if(!exists(key)) return null;
		return untyped h[key];
		#else
		return null;
		#end
	}

	/**
		Tells if a value exists for the given key.
		In particular, it's useful to tells if a key has
		a [null] value versus no value.
	**/
	public function exists( key : String ) : Bool {
		#if flash9
		return untyped h.hasOwnProperty("$"+key);
		#elseif flash
		return untyped h["hasOwnProperty"]("$"+key);
		#elseif js
		try {
			key = "$"+key;
			return untyped this.hasOwnProperty.call(h,key);
		}catch(e:Dynamic){
			untyped __js__("
				for(var i in this.h)
					if( i == key ) return true;
			");
			return false;
		}
		#elseif neko
		return untyped __dollar__hmem(h,key.__s,null);
		#elseif php
		return untyped __php__("array_key_exists")(key, h);
		#else
		return false;
		#end
	}

	/**
		Removes a hashtable entry. Returns [true] if
		there was such entry.
	**/
	public function remove( key : String ) : Bool {
		#if flash9
		key = "$"+key;
		if( untyped !h.hasOwnProperty(key) ) return false;
		untyped __delete__(h,key);
		return true;
		#elseif flash
		key = "$"+key;
		if( untyped !h["hasOwnProperty"](key) ) return false;
		untyped __delete__(h,key);
		return true;
		#elseif js
		if( !exists(key) )
			return false;
		untyped __js__("delete")(h["$"+key]);
		return true;
		#elseif neko
		return untyped __dollar__hremove(h,key.__s,null);
		#elseif php
		return php.Boot.__array_remove_at(cast h, untyped key);
		#else
		return false;
		#end
	}

	/**
		Returns an iterator of all keys in the hashtable.
	**/
	public function keys() : Iterator<String> {
		#if flash9
		return untyped (__hkeys__(h)).iterator();
		#elseif flash
		return untyped (__hkeys__(h))["iterator"]();
		#elseif js
		var a = new Array<String>();
		untyped __js__("
			for(var i in this.h)
				a.push(i.substr(1));
		");
		return a.iterator();
		#elseif neko
		var l = new List<String>();
		untyped __dollar__hiter(h,function(k,_) { l.push(new String(k)); });
		return l.iterator();
		#elseif php
		return php.Boot.__array_iterator(untyped __php__("array_keys")(h));
		#else
		return null;
		#end
	}

	/**
		Returns an iterator of all values in the hashtable.
	**/
	public function iterator() : Iterator<T> {
		#if flash9
		return untyped {
			ref : h,
			it : __keys__(h).iterator(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i : Dynamic = this.it.next(); return this.ref[i]; }
		};
		#elseif flash
		return untyped {
			ref : h,
			it : __keys__(h)["iterator"](),
			hasNext : function() { return this.it[__unprotect__("hasNext")](); },
			next : function() { var i = this.it[__unprotect__("next")](); return this.ref[i]; }
		};
		#elseif js
		return untyped {
			ref : h,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref["$"+i]; }
		};
		#elseif neko
		var l = new List<T>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
		#elseif php
		return php.Boot.__array_iterator(untyped __php__("array_values")(h));
		#else
		return null;
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
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
