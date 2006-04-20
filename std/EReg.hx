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

class EReg {

	var r : Void;

	public function new( r : String ) {
		#if neko
		this.r = regexp_new(untyped r.__s);
		#else js
		this.r = untyped __new__("RegExp",r);
		#else flash
		throw "EReg::new not implemented";
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
			trace(r.m);
			return true;
		}
		#else flash
		throw "EReg::match not implemented";
		return false;
		#else error
		#end
	}

	public function matched( n : Int ) : String {
		#if neko
		return new String(regexp_matched(r,n));
		#else js
		return untyped if( r.m != null && n >= 0 && n < r.m.length ) r.m[n] else throw "EReg::matched";
		#else flash
		throw "EReg::matched not implemented";
		return "";
		#else error
		#end
	}

	public function matchedPos() : { pos : Int, len : Int } {
		#if neko
		return regexp_matched_pos(r,0);
		#else js
		if( untyped r.m == null ) throw "EReg::matchedPos";
		return untyped { pos : r.m.index, len : r.m[0].length };
		#else flash
		throw "EReg::matchedPos not implemented";
		return null;
		#else error
		#end
	}

#if neko
	static var regexp_new = neko.Lib.load("regexp","regexp_new",1);
	static var regexp_match = neko.Lib.load("regexp","regexp_match",4);
	static var regexp_matched = neko.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos = neko.Lib.load("regexp","regexp_matched_pos",2);
#end

}
