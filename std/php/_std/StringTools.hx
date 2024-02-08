/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import php.*;
import haxe.iterators.StringIterator;
import haxe.iterators.StringKeyValueIterator;

@:coreApi class StringTools {
	public inline static function urlEncode(s:String):String {
		return Global.rawurlencode(s);
	}

	public inline static function urlDecode(s:String):String {
		return Global.urldecode(s);
	}

	public inline static function htmlEscape(s:String, ?quotes:Bool):String {
		return Global.htmlspecialchars(s, (quotes ? Const.ENT_QUOTES | Const.ENT_HTML401 : Const.ENT_NOQUOTES));
	}

	public inline static function htmlUnescape(s:String):String {
		return Global.htmlspecialchars_decode(s, Const.ENT_QUOTES);
	}

	public inline static function contains(s:String, value:String):Bool {
		return s.indexOf(value) != -1;
	}

	public static function startsWith(s:String, start:String):Bool {
		return start == '' || Global.substr(s, 0, Global.strlen(start)) == start;
	}

	public static function endsWith(s:String, end:String):Bool {
		return end == '' || Global.substr(s, -Global.strlen(end)) == end;
	}

	public static function isSpace(s:String, pos:Int):Bool {
		var c = s.charCodeAt(pos);
		return (c >= 9 && c <= 13) || c == 32;
	}

	public inline static function ltrim(s:String):String {
		return Global.ltrim(s);
	}

	public inline static function rtrim(s:String):String {
		return Global.rtrim(s);
	}

	public inline static function trim(s:String):String {
		return Global.trim(s);
	}

	public static function rpad(s:String, c:String, l:Int):String {
		var cLength = c.length;
		var sLength = s.length;
		if (cLength == 0 || sLength >= l)
			return s;
		var padLength = l - sLength;
		var padCount = Syntax.int(padLength / cLength);
		if (padCount > 0) {
			var result = Global.str_pad(s, Global.strlen(s) + padCount * Global.strlen(c), c, Const.STR_PAD_RIGHT);
			return (padCount * cLength >= padLength) ? result : Syntax.concat(result, c);
		} else {
			return Syntax.concat(s, c);
		}
	}

	public static function lpad(s:String, c:String, l:Int):String {
		var cLength = c.length;
		var sLength = s.length;
		if (cLength == 0 || sLength >= l)
			return s;
		var padLength = l - sLength;
		var padCount = Syntax.int(padLength / cLength);
		if (padCount > 0) {
			var result = Global.str_pad(s, Global.strlen(s) + padCount * Global.strlen(c), c, Const.STR_PAD_LEFT);
			return (padCount * cLength >= padLength) ? result : Syntax.concat(c, result);
		} else {
			return Syntax.concat(c, s);
		}
	}

	public static function replace(s:String, sub:String, by:String):String {
		if (sub == '') {
			return Global.implode(by, Global.preg_split('//u', s, -1, Const.PREG_SPLIT_NO_EMPTY));
		}
		return Global.str_replace(sub, by, s);
	}

	public static function hex(n:Int, ?digits:Int):String {
		var s = Global.dechex(n);
		var len = 8;
		if (Global.strlen(s) > (null == digits ? len : (len = digits > len ? digits : len)))
			s = s.substr(-len);
		else if (digits != null)
			s = lpad(s, '0', digits);
		return s.toUpperCase();
	}

	public static function fastCodeAt(s:String, index:Int):Int {
		var char:NativeString = (index == 0 ? s : Global.mb_substr(s, index, 1));
		if (char == '')
			return 0;
		return Boot.unsafeOrd(char);
	}

	public static function unsafeCodeAt(s:String, index:Int):Int {
		var char:NativeString = (index == 0 ? s : Global.mb_substr(s, index, 1));
		return Boot.unsafeOrd(char);
	}

	public static inline function iterator(s:String):StringIterator {
		return new StringIterator(s);
	}

	public static inline function keyValueIterator(s:String):StringKeyValueIterator {
		return new StringKeyValueIterator(s);
	}

	@:noUsing public static inline function isEof(c:Int):Bool {
		return c == 0;
	}

	@:noCompletion
	@:deprecated('StringTools.quoteUnixArg() is deprecated. Use haxe.SysTools.quoteUnixArg() instead.')
	public static function quoteUnixArg(argument:String):String {
		return inline haxe.SysTools.quoteUnixArg(argument);
	}

	@:noCompletion
	@:deprecated('StringTools.winMetaCharacters is deprecated. Use haxe.SysTools.winMetaCharacters instead.')
	public static var winMetaCharacters:Array<Int> = cast haxe.SysTools.winMetaCharacters;

	@:noCompletion
	@:deprecated('StringTools.quoteWinArg() is deprecated. Use haxe.SysTools.quoteWinArg() instead.')
	public static function quoteWinArg(argument:String, escapeMetaCharacters:Bool):String {
		return inline haxe.SysTools.quoteWinArg(argument, escapeMetaCharacters);
	}
}
