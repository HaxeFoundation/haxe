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
	The StringTools class contains some extra functionalities for [String]
	manipulation. It's stored in a different class in order to prevent
	the standard [String] of being bloated and thus increasing the size of
	each application using it.
**/
class StringTools {

	/**
		Encode an URL by using the standard format.
	**/
	public static function urlEncode( s : String ) : String {
		#if flash
		return untyped _global.escape(s);
		#else neko
		return new String(_urlEncode(untyped s.__s));
		#else js
		return untyped encodeURIComponent(s);
		#else error
		#end
	}

	/**
		Decode an URL using the standard format.
	**/
	public static function urlDecode( s : String ) : String {
		#if flash
		return untyped _global.unescape(s);
		#else neko
		return new String(_urlDecode(untyped s.__s));
		#else js
		return untyped decodeURIComponent(s.split("+").join(" "));
		#else error
		#end
	}

	/**
		Escape HTML special characters of the string.
	**/
	public static function htmlEscape( s : String ) : String {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	/**
		Unescape HTML special characters of the string.
	**/
	public static function htmlUnescape( s : String ) : String {
		return s.split("&gt;").join(">").split("&lt;").join("<").split("&amp;").join("&");
	}

	/**
		Tells if the string [s] starts with the string [start].
	**/
	public static function startsWith( s : String, start : String ) {
		return( s.length >= start.length && s.substr(0,start.length) == start );
	}

	/**
		Tells if the string [s] ends with the string [end].
	**/
	public static function endsWith( s : String, end : String ) {
		var elen = end.length;
		var slen = s.length;
		return( slen >= elen && s.substr(slen-elen,elen) == end );
	}

	/**
		Tells if the character in the string [s] at position [pos] is a space.
	**/
	public static function isSpace( s : String, pos : Int ) : Bool {
		var c = s.charCodeAt( pos );
		return (c >= 9 && c <= 13) || c == 32;
	}

	/**
		Removes spaces at the left of the String [s].
	**/
	public static function ltrim( s : String ) : String {
		var l = s.length;
		var r = 0;
		while( r < l && isSpace(s,r) ){
			r++;
		}
		if( r > 0 )
			return s.substr(r, l-r);
		else
			return s;
	}

	/**
		Removes spaces at the right of the String [s].
	**/
	public static function rtrim( s : String ) : String {
		var l = s.length;
		var r = 0;
		while( r < l && isSpace(s,l-r-1) ){
			r++;
		}
		if( r > 0 ){
			return s.substr(0, l-r);
		}else{
			return s;
		}
	}

	/**
		Removes spaces at the beginning and the end of the String [s].
	**/
	public static function trim( s : String ) : String {
		return ltrim(rtrim(s));
	}

	/**
		Pad the string [s] by appending [c] at its right until it reach [l] characters.
	**/
	public static function rpad( s : String, c : String, l : Int ) : String {
		var sl = s.length;
		var cl = c.length;
		while( sl < l ){
			if( l - sl < cl ){
				s += c.substr(0,l-sl);
				sl = l;
			}else{
				s += c;
				sl += cl;
			}
		}
		return s;
	}

	/**
		Pad the string [s] by appending [c] at its left until it reach [l] characters.
	**/
	public static function lpad( s : String, c : String, l : Int ) : String {
		var ns = "";
		var sl = s.length;
		if( sl >= l ) return s;

		var cl = c.length;
		while( sl < l ){
			if( l - sl < cl ){
				ns += c.substr(0,l-sl);
				sl = l;
			}else{
				ns += c;
				sl += cl;
			}
		}
		return ns+s;
	}

	/**
		Replace all occurences of the string [sub] in the string [s] by the string [by].
	**/
	public static function replace( s : String, sub : String, by : String ) : String {
		return s.split(sub).join(by);
	}


	#if neko
	private static var _urlEncode = neko.Lib.load("std","url_encode",1);
	private static var _urlDecode = neko.Lib.load("std","url_decode",1);
	#end

}
