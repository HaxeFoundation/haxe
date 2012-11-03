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
/**
	The StringTools class contains some extra functionalities for [String]
	manipulation. It's stored in a different class in order to prevent
	the standard [String] of being bloated and thus increasing the size of
	each application using it.
**/
#if cs
@:keep
#end
class StringTools {

	/**
		Encode an URL by using the standard format.
	**/
	public static function urlEncode( s : String ) : String untyped {
		#if flash9
			return __global__["encodeURIComponent"](s);
		#elseif flash
			return _global["escape"](s);
		#elseif neko
			return new String(_urlEncode(s.__s));
		#elseif js
			return encodeURIComponent(s);
		#elseif cpp
			return s.__URLEncode();
		#elseif java
			try
				return untyped __java__("java.net.URLEncoder.encode(s, \"UTF-8\")")
			catch (e:Dynamic) throw e;
		#elseif cs
			return untyped __cs__("System.Uri.EscapeUriString(s)");
		#else
			return null;
		#end
	}

	/**
		Decode an URL using the standard format.
	**/
	public static function urlDecode( s : String ) : String untyped {
		#if flash9
			return __global__["decodeURIComponent"](s.split("+").join(" "));
		#elseif flash
			return _global["unescape"](s);
		#elseif neko
			return new String(_urlDecode(s.__s));
		#elseif js
			return decodeURIComponent(s.split("+").join(" "));
		#elseif cpp
			return s.__URLDecode();
		#elseif java
			try
				return untyped __java__("java.net.URLDecoder.decode(s, \"UTF-8\")")
			catch (e:Dynamic) throw e;
		#elseif cs
			return untyped __cs__("System.Uri.UnescapeDataString(s)");
		#else
			return null;
		#end
	}

	/**
		Escape HTML special characters of the string.
	**/
	public static function htmlEscape( s : String, ?quotes : Bool ) : String {
		s = s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
		return quotes ? s.split('"').join("&quot;").split("'").join("&#039;") : s;
	}

	/**
		Unescape HTML special characters of the string.
	**/
	public static function htmlUnescape( s : String ) : String {
		return s.split("&gt;").join(">").split("&lt;").join("<").split("&quot;").join('"').split("&#039;").join("'").split("&amp;").join("&");
	}

	/**
		Tells if the string [s] starts with the string [start].
	**/
	public static #if (cs || java) inline #end function startsWith( s : String, start : String ) : Bool {
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
	public static #if (cs || java) inline #end function endsWith( s : String, end : String ) : Bool {
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
	public #if cs inline #end static function ltrim( s : String ) : String {
		#if cs
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
	public #if cs inline #end static function rtrim( s : String ) : String {
		#if cs
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
	public #if (cs || java) inline #end static function trim( s : String ) : String {
		#if cs
		return untyped s.Trim();
		#elseif java
		return untyped s.trim();
		#else
		return ltrim(rtrim(s));
		#end
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
	public #if (java || cs) inline #end static function replace( s : String, sub : String, by : String ) : String {
		#if java
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
	public static inline function fastCodeAt( s : String, index : Int ) : Int untyped {
		#if neko
		return untyped __dollar__sget(s.__s, index);
		#elseif cpp
		return s.cca(index);
		#elseif flash9
		return s.cca(index);
		#elseif flash
		return s["cca"](index);
		#elseif java
		return ( index < s.length ) ? cast(_charAt(s, index), Int) : -1;
		#elseif cs
		return ( cast(index, UInt) < s.length ) ? cast(untyped s[index], Int) : -1;
		#elseif js
			#if mt
		return (untyped s).cca(index);
			#else
		return (untyped s).charCodeAt(index);
			#end
		#else
		return s.cca(index);
		#end
	}

	#if java
	private static inline function _charAt(str:String, idx:Int):java.StdTypes.Char16 return untyped str._charAt(idx)
	#end
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
		return c == -1;
		#elseif java
		return c == -1;
		#else
		return false;
		#end
	}

	#if neko
	private static var _urlEncode = neko.Lib.load("std","url_encode",1);
	private static var _urlDecode = neko.Lib.load("std","url_decode",1);
	#end

}
