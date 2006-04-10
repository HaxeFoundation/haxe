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
package tools;

class EReg {

	var r : Void;

	public function new( r : String ) {
		#if neko
		this.r = regexp_new(untyped r.__s);
		#else js
		this.r = untyped __new__("RegExp",r);
		#else error
		#end
	}

	public function match( s : String ) : Bool {
		#if neko
		return regexp_match(r,untyped s.__s,0,s.length);
		#else js
		untyped {
			r.m = r.exec(s);
			if( r.m == null )
				return false;
			r.m.push(__js__("RegExp.$1"));
			r.m.push(__js__("RegExp.$2"));
			r.m.push(__js__("RegExp.$3"));
			r.m.push(__js__("RegExp.$4"));
			r.m.push(__js__("RegExp.$5"));
			r.m.push(__js__("RegExp.$6"));
			r.m.push(__js__("RegExp.$7"));
			r.m.push(__js__("RegExp.$8"));
			r.m.push(__js__("RegExp.$9"));
			return true;
		}
		#else error
		#end
	}

	public function matched( n : Int ) : String {
		#if neko
		return new String(regexp_matched(r,n));
		#else js
		return untyped if( r.m != null ) r.m[n] else throw "EReg::matched";
		#else error
		#end
	}

#if neko
	public function matchedPos( n : Int) : { pos : Int, len : Int } {
		return regexp_matched_pos(r,n);
	}
	// not available in JS....
#end

#if neko
	static var regexp_new = neko.Lib.load("regexp","regexp_new",1);
	static var regexp_match = neko.Lib.load("regexp","regexp_match",4);
	static var regexp_matched = neko.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos = neko.Lib.load("regexp","regexp_matched_pos",2);
#end

}
