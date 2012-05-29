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
	public #if php inline #end static function urlEncode( s : String ) : String untyped {
		#if flash9
			return __global__["encodeURIComponent"](s);
		#elseif flash
			return _global["escape"](s);
		#elseif neko
			return new String(_urlEncode(s.__s));
		#elseif js
			return encodeURIComponent(s);
		#elseif php
			return __call__("rawurlencode", s);
		#elseif cpp
			return s.__URLEncode();
		#elseif java
			return untyped __java__("java.net.URLEncoder.encode(s)");
		#elseif cs
			return untyped __cs__("System.Uri.EscapeUriString(s)");
		#else
			return null;
		#end
	}

	/**
		Decode an URL using the standard format.
	**/
	public #if php inline #end static function urlDecode( s : String ) : String untyped {
		#if flash9
			return __global__["decodeURIComponent"](s.split("+").join(" "));
		#elseif flash
			return _global["unescape"](s);
		#elseif neko
			return new String(_urlDecode(s.__s));
		#elseif js
			return decodeURIComponent(s.split("+").join(" "));
		#elseif php
			return __call__("urldecode", s);
		#elseif cpp
			return s.__URLDecode();
		#elseif java
			return untyped __java__("java.net.URLDecoder.decode(s)");
		#elseif cs
			return untyped __cs__("System.Uri.UnescapeDataString(s)");
		#else
			return null;
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
	public #if php inline #end static function htmlUnescape( s : String ) : String {
		#if php
		return untyped __call__("htmlspecialchars_decode", s);
		#else
		return s.split("&gt;").join(">").split("&lt;").join("<").split("&amp;").join("&");
		#end
	}

	/**
		Tells if the string [s] starts with the string [start].
	**/
	public static #if (java || cs) inline #end function startsWith( s : String, start : String ) {
		#if java
		return untyped s.startsWith(start);
		#elseif cs
		return untyped s.StartsWith(start);
		#else
		return( s.length >= start.length && s.substr(0, start.length) == start );
		#end
	}

	/**
		Tells if the string [s] ends with the string [end].
	**/
	public static #if (java || cs) inline #end function endsWith( s : String, end : String ) {
		#if java
		return untyped s.endsWith(end);
		#elseif cs
		return untyped s.EndsWith(end);
		#else
		var elen = end.length;
		var slen = s.length;
		return( slen >= elen && s.substr(slen - elen, elen) == end );
		#end
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
	public #if (php || cs) inline #end static function ltrim( s : String ) : String {
		#if php
		return untyped __call__("ltrim", s);
		#elseif cs
		return untyped s.TrimStart();
		#else
		var l = s.length;
		var r = 0;
		while( r < l && isSpace(s,r) ){
			r++;
		}
		if( r > 0 )
			return s.substr(r, l-r);
		else
			return s;
		#end
	}

	/**
		Removes spaces at the right of the String [s].
	**/
	public #if (php || cs) inline #end static function rtrim( s : String ) : String {
		#if php
		return untyped __call__("rtrim", s);
		#elseif cs
		return untyped s.TrimEnd();
		#else
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
		#end
	}

	/**
		Removes spaces at the beginning and the end of the String [s].
	**/
	public #if (php || cs) inline #end static function trim( s : String ) : String {
		#if php
		return untyped __call__("trim", s);
		#elseif cs
		return untyped s.Trim();
		#else
		return ltrim(rtrim(s));
		#end
	}

	/**
		Pad the string [s] by appending [c] at its right until it reach [l] characters.
	**/
	public #if php inline #end static function rpad( s : String, c : String, l : Int ) : String {
		#if php
		return untyped __call__("str_pad", s, l, c, __php__("STR_PAD_RIGHT"));
		#else
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
		#end
	}

	/**
		Pad the string [s] by appending [c] at its left until it reach [l] characters.
	**/
	public #if php inline #end static function lpad( s : String, c : String, l : Int ) : String {
		#if php
		return untyped __call__("str_pad", s, l, c, __php__("STR_PAD_LEFT"));
		#else
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
		#end
	}

	/**
		Replace all occurences of the string [sub] in the string [s] by the string [by].
	**/
	public #if (php || java) inline #end static function replace( s : String, sub : String, by : String ) : String {
		#if php
		return untyped __call__("str_replace", sub, by, s);
		#elseif java
		return untyped s.replace(sub, by);
		#elseif cs
		return untyped s.Replace(sub, by);
		#else
		return s.split(sub).join(by);
		#end
	}

	/**
		Encode a number into a hexadecimal representation, with an optional number of zeros for left padding.
	**/
	public static function hex( n : Int, ?digits : Int ) {
		#if flash9
			var n : UInt = n;
			var s : String = untyped n.toString(16);
			s = s.toUpperCase();
		#else
			var s = "";
			var hexChars = "0123456789ABCDEF";
			do {
				s = hexChars.charAt(n&15) + s;
				n >>>= 4;
			} while( n > 0 );
		#end
		if( digits != null )
			while( s.length < digits )
				s = "0"+s;
		return s;
	}

	/**
		Provides a fast native string charCodeAt access. Since the EOF value might vary depending on the platforms, always test with StringTools.isEOF.
		Only guaranteed to work if index in [0,s.length] range. Might not work with strings containing \0 char.
	**/
		public static #if !cs inline #end function fastCodeAt( s : String, index : Int ) : Int untyped {
		#if neko
		return untyped __dollar__sget(s.__s, index);
		#elseif cpp
		return s.cca(index);
		#elseif flash9
		return s.cca(index);
		#elseif flash
		return s["cca"](index);
		#elseif java
		return s.charCodeAt(index);
		#elseif cs
		if (cast(index, UInt) >= s.length)
			return 0;
		else
			return cast(untyped s[index], Int);
		#elseif js
		return (untyped s).charCodeAt(index);
		#else
		return s.cca(index);
		#end
	}

	/*
		Only to use together with fastCodeAt.
	*/
	public static inline function isEOF( c : Int ) : Bool {
		#if (flash9 || cpp)
		return c == 0;
		#elseif flash8
		return c <= 0; // fast NaN
		#elseif js
		return c != c; // fast NaN
		#elseif neko
		return c == null;
		#elseif cs
		return c == 0;
		#else
		return false;
		#end
	}

	#if neko
	private static var _urlEncode = neko.Lib.load("std","url_encode",1);
	private static var _urlDecode = neko.Lib.load("std","url_decode",1);
	#end

}
