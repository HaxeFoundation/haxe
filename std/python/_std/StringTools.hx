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
	This class provides advanced methods on Strings. It is ideally used with
	'using StringTools' and then acts as an extension to the String class.

	If the first argument to any of the methods is null, the result is
	unspecified.
**/

class StringTools {
	/**
		Encode an URL by using the standard format.
	**/
	public static function urlEncode( s : String ) : String untyped {
		__python__("from urllib.parse import quote");
		return untyped quote(s);
	}

	/**
		Decode an URL using the standard format.
	**/
	public static function urlDecode( s : String ) : String untyped {
		__python__("from urllib.parse import unquote");
		
		return untyped unquote(s);
	}

	/**
		Escapes HTML special characters of the string `s`.

		The following replacements are made:
			
		- `&` becomes `&amp`;
		- `<` becomes `&lt`;
		- `>` becomes `&gt`;
		
		If `quotes` is true, the following characters are also replaced:
		
		- `"` becomes `&quot`;
		- `'` becomes `&#039`;
	**/
	public static function htmlEscape( s : String, ?quotes : Bool ) : String {
		s = s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
		return quotes ? s.split('"').join("&quot;").split("'").join("&#039;") : s;
	}

	/**
		Unescapes HTML special characters of the string `s`.

		This is the inverse operation to htmlEscape, i.e. the following always
		holds: htmlUnescape(htmlEscape(s)) == s

		The replacements follow:
			
		- `&amp;` becomes `&`
		- `&lt;` becomes `<`
		- `&gt;` becomes `>`
		- `&quot;` becomes `"`
		- `&#039;` becomes `'`
	**/
	public static function htmlUnescape( s : String ) : String {
		return s.split("&gt;").join(">").split("&lt;").join("<").split("&quot;").join('"').split("&#039;").join("'").split("&amp;").join("&");
	}

	/**
		Tells if the string `s` starts with the string `start`.

		If `start` is null, the result is unspecified.

		If `start` is the empty String "", the result is true.
	**/
	public static #if (cs || java) inline #end function startsWith( s : String, start : String ) : Bool {
		return( s.length >= start.length && s.substr(0, start.length) == start );
	}

	/**
		Tells if the string `s` ends with the string `end`.

		If `end` is null, the result is unspecified.

		If `end` is the empty String "", the result is true.
	**/
	public static #if (cs || java) inline #end function endsWith( s : String, end : String ) : Bool {
		var elen = end.length;
		var slen = s.length;
		return( slen >= elen && s.substr(slen - elen, elen) == end );
	}

	/**
		Tells if the character in the string `s` at position `pos` is a space.

		A character is considered to be a space character if its character code
		is 9,10,11,12,13 or 32.

		If `s` is the empty String "", or if pos is not a valid position within
		`s`, the result is false.
	**/
	public static function isSpace( s : String, pos : Int ) : Bool {
		if (s.length == 0 || pos < 0 || pos >= s.length) return false;
		var c = s.charCodeAt( pos );
		return (c > 8 && c < 14) || c == 32;
	}

	/**
		Removes leading space characters of `s`.

		This function internally calls isSpace() to decide which characters to
		remove.

		If `s` is the empty String "" or consists only of space characters, the
		result is the empty String "".
	**/
	public #if cs inline #end static function ltrim( s : String ) : String {
		
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
		Removes trailing space characters of `s`.

		This function internally calls isSpace() to decide which characters to
		remove.

		If `s` is the empty String "" or consists only of space characters, the
		result is the empty String "".
	**/
	public #if cs inline #end static function rtrim( s : String ) : String {
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
		Removes leading and trailing space characters of `s`.

		This is a convenience function for ltrim(rtrim(s)).
	**/
	public static function trim( s : String ) : String {
		return ltrim(rtrim(s));
	}

	/**
		Concatenates `c` to `s` until `s.length` is at least `l`.

		If `c` is the empty String "" or if `l` does not exceed `s.length`,
		`s` is returned unchanged.

		If `c.length` is 1, the resulting String length is exactly `l`.

		Otherwise the length may exceed `l`.

		If `c` is null, the result is unspecified.
	**/
	public static function lpad( s : String, c : String, l : Int ) : String {
		if (c.length <= 0)
			return s;

		while (s.length < l) {
			s = c + s;
		}
		return s;
	}

	/**
		Appends `c` to `s` until `s.length` is at least `l`.

		If `c` is the empty String "" or if `l` does not exceed `s.length`,
		`s` is returned unchanged.

		If `c.length` is 1, the resulting String length is exactly `l`.

		Otherwise the length may exceed `l`.

		If `c` is null, the result is unspecified.
	**/
	public static function rpad( s : String, c : String, l : Int ) : String {
		if (c.length <= 0)
			return s;

		while (s.length < l) {
			s = s + c;
		}
		return s;
	}

	/**
		Replace all occurences of the String `sub` in the String `s` by the
		String `by`.

		If `sub` is the empty String "", `by` is inserted after each character
		of `s`. If `by` is also the empty String "", `s` remains unchanged.

		This is a convenience function for `s.split(sub).join(by)`.

		If `sub` or `by` are null, the result is unspecified.
	**/
	public static function replace( s : String, sub : String, by : String ) : String {
		return s.split(sub).join(by);
	}

	/**
		Encodes `n` into a hexadecimal representation.

		If `digits` is specified, the resulting String is padded with "0" until
		its length equals `digits`.
	**/
	public static function hex( n : Int, ?digits : Int ) {

		var s = "";
		var hexChars = "0123456789ABCDEF";
		do {
			s = hexChars.charAt(n&15) + s;
			n >>>= 4;
		} while( n > 0 );
		if (digits != null && s.length < digits) {
			var diff = digits - s.length;
			for (_ in 0...diff) {
				s = "0" + s;
			}
		}
		return s;
		
	}

	/**
		Returns the character code at position `index` of String `s`.

		This method is faster than String.charCodeAt() on most platforms.
		However, unlike String.charCodeAt(), the result is unspecified if
		`index` is negative or exceeds `s.length`.

		This operation is not guaranteed to work if `s` contains the \0
		character.
	**/
	public static inline function fastCodeAt( s : String, index : Int ) : Int untyped {
		
		return if (index >= s.length) -1 else untyped(ord(untyped s[index]));
	}

	/*
		Tells if `c` represents the end-of-file (EOF) character.
	*/
	@:noUsing public static inline function isEof( c : Int ) : Bool {
		return c == -1;
	}



}