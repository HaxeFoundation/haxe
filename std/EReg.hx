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
	Regular expressions are a way to find regular patterns into
	Strings. Have a look at the tutorial on haXe website to learn
	how to use them.
**/
class EReg {

	var r : Dynamic;
	#if flash9
	var result : {> Array<String>, index : Int, input : String };
	#end
	#if (neko || php || cpp)
	var last : String;
	var global : Bool;
	#end
	#if php
	var pattern : String;
	var options : String;
	var re : String;
	var matches : ArrayAccess<Dynamic>;
	#end

	/**
		Creates a new regular expression with pattern [r] and
		options [opt].
	**/
	public function new( r : String, opt : String ) {
		#if cpp
			var a = opt.split("g");
			global = a.length > 1;
			if( global )
				opt = a.join("");
			this.r = regexp_new_options(r, opt);
		#elseif js
			opt = opt.split("u").join(""); // 'u' (utf8) depends on page encoding
			this.r = untyped __new__("RegExp",r,opt);
		#elseif flash9
			this.r = untyped __new__(__global__["RegExp"],r,opt);
		#elseif php
			this.pattern = r;
			var a = opt.split("g");
			global = a.length > 1;
			if( global )
				opt = a.join("");
			this.options = opt;
			this.re = "/" + untyped __php__("str_replace")("/", "\\/", r) + "/" + opt;
		#else
			throw "Regular expressions are not implemented for this platform";
		#end
	}

	/**
		Tells if the regular expression matches the String.
		Updates the internal state accordingly.
	**/
	public function match( s : String ) : Bool {
		#if cpp
			var p = regexp_match(r,s,0,s.length);
			if( p )
				last = s;
			else
				last = null;
			return p;
		#elseif js
			untyped {
				r.m = r.exec(s);
				r.s = s;
				r.l = RegExp.leftContext;
				r.r = RegExp.rightContext;
				return (r.m != null);
			}
		#elseif flash9
			result = untyped r.exec(s);
			return (result != null);
		#elseif php
			var p : Int = untyped __php__("preg_match")(re, s, matches, __php__("PREG_OFFSET_CAPTURE"));
			if(p > 0)
				last = s;
			else
				last = null;
			return p > 0;
		#else
			return false;
		#end
	}

	/**
		Returns a matched group or throw an expection if there
		is no such group. If [n = 0], the whole matched substring
		is returned.
	**/
	public function matched( n : Int ) : String {
		#if cpp
			var m = regexp_matched(r,n);
			return m;
		#elseif js
			return untyped if( r.m != null && n >= 0 && n < r.m.length ) r.m[n] else throw "EReg::matched";
		#elseif flash9
			return untyped if( result != null && n >= 0 && n < result.length ) result[n] else throw "EReg::matched";
		#elseif php
			if( n < 0 ) throw "EReg::matched";
			// we can't differenciate between optional groups at the end of a match
			// that have not been matched and invalid groups
			if( n >= untyped __call__("count", matches)) return null;
			if(untyped __php__("$this->matches[$n][1] < 0")) return null;
			return untyped __php__("$this->matches[$n][0]");
		#else
			return null;
		#end
	}

	/**
		Returns the part of the string that was as the left of
		of the matched substring.
	**/
	public function matchedLeft() : String {
		#if cpp
			var p = regexp_matched_pos(r,0);
			return last.substr(0,p.pos);
		#elseif js
			untyped {
				if( r.m == null ) throw "No string matched";
				if( r.l == null ) return r.s.substr(0,r.m.index);
				return r.l;
			}
		#elseif flash9
			if( result == null ) throw "No string matched";
			var s = result.input;
			return s.substr(0,result.index);
		#elseif php
			if( untyped __call__("count", matches) == 0 ) throw "No string matched";
			return last.substr(0, untyped __php__("$this->matches[0][1]"));
		#else
			return null;
		#end
	}

	/**
		Returns the part of the string that was at the right of
		of the matched substring.
	**/
	public function matchedRight() : String {
		#if cpp
			var p = regexp_matched_pos(r,0);
			var sz = p.pos+p.len;
			return last.substr(sz,last.length-sz);
		#elseif js
			untyped {
				if( r.m == null ) throw "No string matched";
				if( r.r == null ) {
					var sz = r.m.index+r.m[0].length;
					return r.s.substr(sz,r.s.length-sz);
				}
				return r.r;
			}
		#elseif flash9
			if( result == null ) throw "No string matched";
			var rl = result.index + result[0].length;
			var s = result.input;
			return s.substr(rl,s.length - rl);
		#elseif php
			if( untyped __call__("count", matches) == 0 ) throw "No string matched";
			var x : Int = untyped __php__("$this->matches[0][1]") + __php__("strlen")(__php__("$this->matches[0][0]"));
			return last.substr(x);
		#else
			return null;
		#end
	}

	/**
		Returns the position of the matched substring within the
		original matched string.
	**/
	public function matchedPos() : { pos : Int, len : Int } {
		#if cpp
			return regexp_matched_pos(r,0);
		#elseif js
			if( untyped r.m == null ) throw "No string matched";
			return untyped { pos : r.m.index, len : r.m[0].length };
		#elseif flash9
			if( result == null ) throw "No string matched";
			return { pos : result.index, len : result[0].length };
		#elseif php
			return untyped { pos : __php__("$this->matches[0][1]"), len : __php__("strlen")(__php__("$this->matches[0][0]")) };
		#else
			return null;
		#end
	}

	/**
		Split a string by using the regular expression to match
		the separators.
	**/
	public function split( s : String ) : Array<String> {
		#if cpp
			var pos = 0;
			var len = s.length;
			var a = new Array();
			var first = true;
			do {
				if( !regexp_match(r,s,pos,len) )
					break;
				var p = regexp_matched_pos(r,0);
				if( p.len == 0 && !first ) {
					if( p.pos == s.length )
						break;
					p.pos += 1;
				}
				a.push(s.substr(pos,p.pos - pos));
				var tot = p.pos + p.len - pos;
				pos += tot;
				len -= tot;
				first = false;
			} while( global );
			a.push(s.substr(pos,len));
			return a;
		#elseif (js || flash9)
			// we can't use directly s.split because it's ignoring the 'g' flag
			var d = "#__delim__#";
			return untyped s.replace(r,d).split(d);
		#elseif php
			return untyped __php__("new _hx_array(preg_split($this->re, $s, $this->hglobal ? -1 : 2))");
		#else
			return null;
		#end
	}

	/**
		Replaces a pattern by another string. The [by] format can
		contains [$1] to [$9] that will correspond to groups matched
		while replacing. [$$] means the [$] character.
	**/
	public function replace( s : String, by : String ) : String {
		#if cpp
			var b = new StringBuf();
			var pos = 0;
			var len = s.length;
			var a = by.split("$");
			var first = true;
			do {
				if( !regexp_match(r,s,pos,len) )
					break;
				var p = regexp_matched_pos(r,0);
				if( p.len == 0 && !first ) {
					if( p.pos == s.length )
						break;
					p.pos += 1;
				}
				b.addSub(s,pos,p.pos-pos);
				if( a.length > 0 )
					b.add(a[0]);
				var i = 1;
				while( i < a.length ) {
					var k = a[i];
					var c = k.charCodeAt(0);
					// 1...9
					if( c >= 49 && c <= 57 ) {
						var p = try regexp_matched_pos(r,Std.int(c)-48) catch( e : String ) null;
						if( p == null ){
							b.add("$");
							b.add(k);
						}else{
						b.addSub(s,p.pos,p.len);
						b.addSub(k,1,k.length - 1);
						}
					} else if( c == null ) {
						b.add("$");
						i++;
						var k2 = a[i];
						if( k2 != null && k2.length > 0 )
							b.add(k2);
					} else
						b.add("$"+k);
					i++;
				}
				var tot = p.pos + p.len - pos;
				pos += tot;
				len -= tot;
				first = false;
			} while( global );
			b.addSub(s,pos,len);
			return b.toString();
		#elseif (js || flash9)
			return untyped s.replace(r,by);
		#elseif php
			by = untyped __call__("str_replace", "$$", "\\$", by);
			untyped __php__("if(!preg_match('/\\\\([^?].+?\\\\)/', $this->re)) $by = preg_replace('/\\$(\\d+)/', '\\\\\\$\\1', $by)");
			return untyped __php__("preg_replace")(re, by, s, global ? -1 : 1);
		#else
			return null;
		#end
	}

	/**
		For each occurence of the pattern in the string [s], the function [f] is called and
		can return the string that needs to be replaced. All occurences are matched anyway,
		and setting the [g] flag might cause some incorrect behavior on some platforms.
	**/
	public function customReplace( s : String, f : EReg -> String ) : String {
		var buf = new StringBuf();
		while( true ) {
			if( !match(s) )
				break;
			buf.add(matchedLeft());
			buf.add(f(this));
			s = matchedRight();
		}
		buf.add(s);
		return buf.toString();
	}

#if cpp
	static var regexp_new_options : String -> String -> Dynamic = cpp.Lib.load("regexp","regexp_new_options",2);
	static var regexp_match : Dynamic -> String -> Int -> Int -> Dynamic = cpp.Lib.load("regexp","regexp_match",4);
	static var regexp_matched : Dynamic -> Int -> Dynamic = cpp.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos : Dynamic -> Int -> { pos : Int, len : Int } = cpp.Lib.load("regexp","regexp_matched_pos",2);
#end

}
