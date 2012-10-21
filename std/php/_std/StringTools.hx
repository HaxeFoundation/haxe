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

@:coreApi class StringTools {

	public inline static function urlEncode( s : String ) : String untyped {
		return __call__("rawurlencode", s);
	}

	public inline static function urlDecode( s : String ) : String untyped {
		return __call__("urldecode", s);
	}

	public static function htmlEscape( s : String ) : String {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	public inline static function htmlUnescape( s : String ) : String {
		return untyped __call__("htmlspecialchars_decode", s);
	}

	public static function startsWith( s : String, start : String ) : Bool {
		return( s.length >= start.length && s.substr(0,start.length) == start );
	}

	public static function endsWith( s : String, end : String ) : Bool {
		var elen = end.length;
		var slen = s.length;
		return( slen >= elen && s.substr(slen-elen,elen) == end );
	}

	public static function isSpace( s : String, pos : Int ) : Bool {
		var c = s.charCodeAt( pos );
		return (c >= 9 && c <= 13) || c == 32;
	}

	public inline static function ltrim( s : String ) : String {
		return untyped __call__("ltrim", s);
	}

	public inline static function rtrim( s : String ) : String {
		return untyped __call__("rtrim", s);
	}

	public inline static function trim( s : String ) : String {
		return untyped __call__("trim", s);
	}

	public inline static function rpad( s : String, c : String, l : Int ) : String {
		return untyped __call__("str_pad", s, l, c, __php__("STR_PAD_RIGHT"));
	}

	public inline static function lpad( s : String, c : String, l : Int ) : String {
		return untyped __call__("str_pad", s, l, c, __php__("STR_PAD_LEFT"));
	}

	public inline static function replace( s : String, sub : String, by : String ) : String {
		return untyped __call__("str_replace", sub, by, s);
	}

	public static function hex( n : Int, ?digits : Int ) : String {
		var s : String = untyped __call__("dechex", n);
		if ( digits != null )
			s = lpad(s, '0', digits);
		return s.toUpperCase();
	}

	public static inline function fastCodeAt( s : String, index : Int ) : Int {
		return untyped s.cca(index);
	}

	public static inline function isEOF( c : Int ) : Bool {
		return untyped __physeq__(c, 0);
	}

}
