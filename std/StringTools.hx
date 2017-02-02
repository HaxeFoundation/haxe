/*
 * Copyright (C)2005-2017 Haxe Foundation
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
	`using StringTools` and then acts as an [extension](https://haxe.org/manual/lf-static-extension.html)
	to the `String` class.

	If the first argument to any of the methods is null, the result is
	unspecified.
**/
#if cpp
using cpp.NativeString;
#end
class StringTools {
	/**
		Encode an URL by using the standard format.
	**/
	#if (!java && !cpp && !lua) inline #end public static function urlEncode( s : String ) : String {
		#if flash
			return untyped __global__["encodeURIComponent"](s);
		#elseif neko
			return untyped new String(_urlEncode(s.__s));
		#elseif js
			return untyped encodeURIComponent(s);
		#elseif cpp
			return untyped s.__URLEncode();
		#elseif java
			return postProcessUrlEncode(java.net.URLEncoder.encode(s, "UTF-8"));
		#elseif cs
			return untyped cs.system.Uri.EscapeDataString(s);
		#elseif python
			return python.lib.urllib.Parse.quote(s, "");
		#elseif hl
			var len = 0;
			var b = @:privateAccess s.bytes.urlEncode(len);
			return @:privateAccess String.__alloc__(b,len);
		#elseif lua
			s = lua.NativeStringTools.gsub(s, "\n", "\r\n");
			s = lua.NativeStringTools.gsub(s, "([^%w %-%_%.%~])", function (c) {
				return lua.NativeStringTools.format("%%%02X", lua.NativeStringTools.byte(c) + '');
			});
			s = lua.NativeStringTools.gsub(s, " ", "+");
			return s;
		#else
			return null;
		#end
	}

#if java
	private static function postProcessUrlEncode( s : String ) : String {
		var ret = new StringBuf();
		var i = 0,
		    len = s.length;
		while (i < len) {
			switch(_charAt(s, i++)) {
			case '+'.code:
				ret.add('%20');
			case '%'.code if (i <= len - 2):
				var c1 = _charAt(s, i++),
				    c2 = _charAt(s, i++);
				switch[c1, c2] {
				case ['2'.code, '1'.code]:
					ret.addChar('!'.code);
				case ['2'.code, '7'.code]:
					ret.addChar('\''.code);
				case ['2'.code, '8'.code]:
					ret.addChar('('.code);
				case ['2'.code, '9'.code]:
					ret.addChar(')'.code);
				case ['7'.code, 'E'.code] | ['7'.code, 'e'.code]:
					ret.addChar('~'.code);
				case _:
					ret.addChar('%'.code);
					ret.addChar(cast c1);
					ret.addChar(cast c2);
				}
			case chr:
				ret.addChar(cast chr);
			}
		}
		return ret.toString();
	}
#end

	/**
		Decode an URL using the standard format.
	**/
	#if (!java && !cpp && !lua) inline #end public static function urlDecode( s : String ) : String {
		#if flash
			return untyped __global__["decodeURIComponent"](s.split("+").join(" "));
		#elseif neko
			return untyped new String(_urlDecode(s.__s));
		#elseif js
			return untyped decodeURIComponent(s.split("+").join(" "));
		#elseif cpp
			return untyped s.__URLDecode();
		#elseif java
			try
				return untyped __java__("java.net.URLDecoder.decode(s, \"UTF-8\")")
			catch (e:Dynamic) throw e;
		#elseif cs
			return untyped cs.system.Uri.UnescapeDataString(s);
		#elseif python
			return python.lib.urllib.Parse.unquote(s);
		#elseif hl
			var len = 0;
			var b = @:privateAccess s.bytes.urlDecode(len);
			return @:privateAccess String.__alloc__(b,len);
		#elseif lua
			s = lua.NativeStringTools.gsub (s, "+", " ");
			s = lua.NativeStringTools.gsub (s, "%%(%x%x)",
				function(h) {return lua.NativeStringTools.char(lua.Lua.tonumber(h,16));});
			s = lua.NativeStringTools.gsub (s, "\r\n", "\n");
			return s;
		#else
			return null;
		#end
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
		holds: `htmlUnescape(htmlEscape(s)) == s`

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

		If `start` is `null`, the result is unspecified.

		If `start` is the empty String `""`, the result is true.
	**/
	public static #if (cs || java) inline #end function startsWith( s : String, start : String ) : Bool {
		#if java
		return untyped s.startsWith(start);
		#elseif cs
		return untyped s.StartsWith(start);
		#elseif cpp
		if (s.length<start.length)
			return false;
		var p0 = s.c_str();
		var p1 = start.c_str();
		for(i in 0...start.length)
			if ( p0.at(i) != p1.at(i) )
				return false;
		return true;
		#elseif hl
		return @:privateAccess (s.length >= start.length && s.bytes.compare(0,start.bytes,0,start.length<<1) == 0);
		#else
		return( s.length >= start.length && s.substr(0, start.length) == start );
		#end
	}

	/**
		Tells if the string `s` ends with the string `end`.

		If `end` is `null`, the result is unspecified.

		If `end` is the empty String `""`, the result is true.
	**/
	public static #if (cs || java) inline #end function endsWith( s : String, end : String ) : Bool {
		#if java
		return untyped s.endsWith(end);
		#elseif cs
		return untyped s.EndsWith(end);
		#elseif cpp
		if (s.length<end.length)
			return false;
		var p0 = s.c_str().add( s.length-end.length );
		var p1 = end.c_str();
		for(i in 0...end.length)
			if ( p0.at(i) != p1.at(i) )
				return false;
		return true;
		#elseif hl
		var elen = end.length;
		var slen = s.length;
		return @:privateAccess (slen >= elen && s.bytes.compare((slen - elen) << 1, end.bytes, 0, elen << 1) == 0);
		#else
		var elen = end.length;
		var slen = s.length;
		return( slen >= elen && s.substr(slen - elen, elen) == end );
		#end
	}

	/**
		Tells if the character in the string `s` at position `pos` is a space.

		A character is considered to be a space character if its character code
		is 9,10,11,12,13 or 32.

		If `s` is the empty String `""`, or if pos is not a valid position within
		`s`, the result is false.
	**/
	public static function isSpace( s : String, pos : Int ) : Bool {
		#if (python || lua)
		if (s.length == 0 || pos < 0 || pos >= s.length) return false;
		#end
		var c = s.charCodeAt( pos );
		return (c > 8 && c < 14) || c == 32;
	}

	/**
		Removes leading space characters of `s`.

		This function internally calls `isSpace()` to decide which characters to
		remove.

		If `s` is the empty String `""` or consists only of space characters, the
		result is the empty String `""`.
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
		Removes trailing space characters of `s`.

		This function internally calls `isSpace()` to decide which characters to
		remove.

		If `s` is the empty String `""` or consists only of space characters, the
		result is the empty String `""`.
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
		Removes leading and trailing space characters of `s`.

		This is a convenience function for `ltrim(rtrim(s))`.
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
		Concatenates `c` to `s` until `s.length` is at least `l`.

		If `c` is the empty String `""` or if `l` does not exceed `s.length`,
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

		If `c` is the empty String `""` or if `l` does not exceed `s.length`,
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
		Replace all occurrences of the String `sub` in the String `s` by the
		String `by`.

		If `sub` is the empty String `""`, `by` is inserted after each character
		of `s`. If `by` is also the empty String `""`, `s` remains unchanged.

		This is a convenience function for `s.split(sub).join(by)`.

		If `sub` or `by` are null, the result is unspecified.
	**/
	public static function replace( s : String, sub : String, by : String ) : String {
		#if java
		if (sub.length == 0)
			return s.split(sub).join(by);
		else
			return untyped s.replace(sub, by);
		#elseif cs
		if (sub.length == 0)
			return s.split(sub).join(by);
		else
			return untyped s.Replace(sub, by);
		#else
		return s.split(sub).join(by);
		#end
	}

	/**
		Encodes `n` into a hexadecimal representation.

		If `digits` is specified, the resulting String is padded with "0" until
		its `length` equals `digits`.
	**/
	public static function hex( n : Int, ?digits : Int ) {
		#if flash
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
		#if python
		if (digits != null && s.length < digits) {
			var diff = digits - s.length;
			for (_ in 0...diff) {
				s = "0" + s;
			}
		}
		#else
		if( digits != null )
			while( s.length < digits )
				s = "0"+s;
		#end
		return s;
	}

	/**
		Returns the character code at position `index` of String `s`, or an
		end-of-file indicator at if `position` equals `s.length`.

		This method is faster than `String.charCodeAt()` on some platforms, but
		the result is unspecified if `index` is negative or greater than
		`s.length`.

		End of file status can be checked by calling `StringTools.isEof()` with
		the returned value as argument.

		This operation is not guaranteed to work if `s` contains the `\0`
		character.
	**/
	public static inline function fastCodeAt( s : String, index : Int ) : Int {
		#if neko
		return untyped __dollar__sget(s.__s, index);
		#elseif cpp
		return untyped s.cca(index);
		#elseif flash
		return untyped s.cca(index);
		#elseif java
		return ( index < s.length ) ? cast(_charAt(s, index), Int) : -1;
		#elseif cs
		return ( cast(index, UInt) < s.length ) ? cast(s[index], Int) : -1;
		#elseif js
		return (untyped s).charCodeAt(index);
		#elseif python
		return if (index >= s.length) -1 else python.internal.UBuiltins.ord(python.Syntax.arrayAccess(s, index));
		#elseif hl
		return @:privateAccess s.bytes.getUI16(index << 1);
		#elseif lua
		return lua.NativeStringTools.byte(s,index+1);
		#else
		return untyped s.cca(index);
		#end
	}

	/*
		Tells if `c` represents the end-of-file (EOF) character.
	*/
	@:noUsing public static inline function isEof( c : Int ) : Bool {
		#if (flash || cpp || hl)
		return c == 0;
		#elseif js
		return c != c; // fast NaN
		#elseif (neko || lua)
		return c == null;
		#elseif cs
		return c == -1;
		#elseif java
		return c == -1;
		#elseif python
		return c == -1;
		#else
		return false;
		#end
	}

	/**
		Returns a String that can be used as a single command line argument
		on Unix.
		The input will be quoted, or escaped if necessary.
	*/
	public static function quoteUnixArg(argument:String):String {
		// Based on cpython's shlex.quote().
		// https://hg.python.org/cpython/file/a3f076d4f54f/Lib/shlex.py#l278

		if (argument == "")
			return "''";

		if (!~/[^a-zA-Z0-9_@%+=:,.\/-]/.match(argument))
			return argument;

		// use single quotes, and put single quotes into double quotes
		// the string $'b is then quoted as '$'"'"'b'
		return "'" + replace(argument, "'", "'\"'\"'") + "'";
	}

	/**
		Character codes of the characters that will be escaped by `quoteWinArg(_, true)`.
	*/
	public static var winMetaCharacters = [" ".code, "(".code, ")".code, "%".code, "!".code, "^".code, "\"".code, "<".code, ">".code, "&".code, "|".code, "\n".code, "\r".code, ",".code, ";".code];

	/**
		Returns a String that can be used as a single command line argument
		on Windows.
		The input will be quoted, or escaped if necessary, such that the output
		will be parsed as a single argument using the rule specified in
		http://msdn.microsoft.com/en-us/library/ms880421

		Examples:
		```
		quoteWinArg("abc") == "abc";
		quoteWinArg("ab c") == '"ab c"';
		```
	*/
	public static function quoteWinArg(argument:String, escapeMetaCharacters:Bool):String {
		// If there is no space, tab, back-slash, or double-quotes, and it is not an empty string.
		if (!~/^[^ \t\\"]+$/.match(argument)) {

			// Based on cpython's subprocess.list2cmdline().
			// https://hg.python.org/cpython/file/50741316dd3a/Lib/subprocess.py#l620

			var result = new StringBuf();
			var needquote = argument.indexOf(" ") != -1 || argument.indexOf("\t") != -1 || argument == "";

			if (needquote)
				result.add('"');

			var bs_buf = new StringBuf();
			for (i in 0...argument.length) {
				switch (argument.charCodeAt(i)) {
					case "\\".code:
						// Don't know if we need to double yet.
						bs_buf.add("\\");
					case '"'.code:
						// Double backslashes.
						var bs = bs_buf.toString();
						result.add(bs);
						result.add(bs);
						bs_buf = new StringBuf();
						result.add('\\"');
					case c:
						// Normal char
						if (bs_buf.length > 0) {
							result.add(bs_buf.toString());
							bs_buf = new StringBuf();
						}
						result.addChar(c);
				}
			}

			// Add remaining backslashes, if any.
			result.add(bs_buf.toString());

			if (needquote) {
				result.add(bs_buf.toString());
				result.add('"');
			}

			argument = result.toString();
		}

		if (escapeMetaCharacters) {
			var result = new StringBuf();
			for (i in 0...argument.length) {
				var c = argument.charCodeAt(i);
				if (winMetaCharacters.indexOf(c) >= 0) {
					result.addChar("^".code);
				}
				result.addChar(c);
			}
			return result.toString();
		} else {
			return argument;
		}
	}

	#if java
	private static inline function _charAt(str:String, idx:Int):java.StdTypes.Char16 return untyped str._charAt(idx);
	#end

	#if neko
	private static var _urlEncode = neko.Lib.load("std","url_encode",1);
	private static var _urlDecode = neko.Lib.load("std","url_decode",1);
	#end

}
