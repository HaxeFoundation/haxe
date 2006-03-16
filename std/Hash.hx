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

class Hash<T> {

	public function new() : Void {
		#if flash
		h = untyped __new__(_global["Object"]);
		#else neko
		h = untyped __dollar__hnew(0);
		#else js
		h = untyped __new__("Object");
		#else error
		#end
	}

	public function set( key : String, value : T ) : Void {
		#if flash
		untyped h[key] = value;
		#else js
		untyped h[key] = value;
		#else neko
		untyped __dollar__hset(h,key.__s,value,null);
		#else error
		#end
	}

	public function get( key : String ) : T {
		#if flash
		return untyped h[key];
		#else js
		return untyped h[key];
		#else neko
		return untyped __dollar__hget(h,key.__s,null);
		#else error
		#end
	}

	public function exists( key : String ) : Bool {
		#if flash
		return untyped h.hasOwnProperty(key);
		#else js
		return untyped h.hasOwnProperty(key);
		#else neko
		return untyped __dollar__hmem(h,key.__s,null);
		#else error
		#end
	}

	public function keys() : Iterator<String> {
		#if flash
		return untyped (__keys__(h)).iterator();
		#else js
		return untyped js.Boot.__keys(h).iterator();
		#else neko
		var l = new List<String>();
		untyped __dollar__hiter(h,function(k,_) { l.push(new String(k)); });
		return l.iterator();
		#else error
		#end
	}

	public function iterator() : Iterator<T> {
		#if flash
		return untyped({
			ref : h,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref[i]; }
		});
		#else js
		return untyped({
			ref : h,
			it : keys(),
			hasNext : function() { return this.it.hasNext(); },
			next : function() { var i = this.it.next(); return this.ref[i]; }
		});
		#else neko
		var l = new List<T>();
		untyped __dollar__hiter(h,function(_,v) { l.push(v); });
		return l.iterator();
		#else error
		#end
	}

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
