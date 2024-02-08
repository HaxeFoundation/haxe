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

import haxe.extern.EitherType;
import php.*;

@:coreApi final class EReg {
	var r:Dynamic;
	var last:String;
	var global:Bool;
	var pattern:String;
	var options:String;
	var re:String;
	var reUnicode(get, never):String;
	var matches:NativeIndexedArray<NativeIndexedArray<EitherType<Int, String>>>;

	public function new(r:String, opt:String):Void {
		this.pattern = r;
		options = Global.str_replace('g', '', opt);
		global = options != opt;
		options = Global.str_replace('u', '', options);
		this.re = '"' + Global.str_replace('"', '\\"', r) + '"' + options;
	}

	public function match(s:String):Bool {
		return matchFromByte(s, 0);
	}

	inline function matchFromByte(s:String, bytesOffset:Int):Bool {
		var p = Global.preg_match(reUnicode, s, matches, Const.PREG_OFFSET_CAPTURE, bytesOffset);
		if (p == false) {
			handlePregError();
			p = Global.preg_match(re, s, matches, Const.PREG_OFFSET_CAPTURE);
		}
		if ((p : Int) > 0) {
			last = s;
		} else {
			last = null;
		}
		return (p : Int) > 0;
	}

	function handlePregError():Void {
		var e = Global.preg_last_error();
		if (e == Const.PREG_INTERNAL_ERROR) {
			throw 'EReg: internal PCRE error';
		} else if (e == Const.PREG_BACKTRACK_LIMIT_ERROR) {
			throw 'EReg: backtrack limit';
		} else if (e == Const.PREG_RECURSION_LIMIT_ERROR) {
			throw 'EReg: recursion limit';
		} else if (e == Const.PREG_JIT_STACKLIMIT_ERROR) {
			throw 'failed due to limited JIT stack space';
		}
		// else if(e == Const.PREG_BAD_UTF8_ERROR) {
		// 	throw 'EReg: malformed UTF8';
		// } else if(e == Const.PREG_BAD_UTF8_OFFSET_ERROR) {
		// 	throw 'EReg: the offset didn\'t correspond to the begin of a valid UTF-8 code point';
		// }
	}

	public function matched(n:Int):String {
		if (matches == null || n < 0)
			throw "EReg::matched";
		// we can't differentiate between optional groups at the end of a match
		// that have not been matched and invalid groups
		if (n >= Global.count(matches))
			return null;
		if ((matches[n][1] : Int) < 0)
			return null;
		return matches[n][0];
	}

	public function matchedLeft():String {
		if (Global.count(matches) == 0)
			throw "No string matched";
		return Global.substr(last, 0, matches[0][1]);
	}

	public function matchedRight():String {
		if (Global.count(matches) == 0)
			throw "No string matched";
		var x:Int = (matches[0][1] : Int) + Global.strlen(matches[0][0]);
		return Global.substr(last, x);
	}

	public function matchedPos():{pos:Int, len:Int} {
		return {
			pos: Global.mb_strlen(Global.substr(last, 0, matches[0][1])),
			len: Global.mb_strlen(matches[0][0])
		};
	}

	public function matchSub(s:String, pos:Int, len:Int = -1):Bool {
		var subject = len < 0 ? s : s.substr(0, pos + len);
		var p = Global.preg_match(reUnicode, subject, matches, Const.PREG_OFFSET_CAPTURE, pos);
		if (p == false) {
			handlePregError();
			p = Global.preg_match(re, subject, matches, Const.PREG_OFFSET_CAPTURE, pos);
		}
		if ((p : Int) > 0) {
			last = s;
		} else {
			last = null;
		}
		return (p : Int) > 0;
	}

	public function split(s:String):Array<String> {
		var parts:EitherType<Bool, NativeArray> = Global.preg_split(reUnicode, s, (global ? -1 : 2));
		if (parts == false) {
			handlePregError();
			parts = Global.preg_split(re, s, (global ? -1 : 2));
		}
		return @:privateAccess Array.wrap(cast parts);
	}

	public function replace(s:String, by:String):String {
		by = Global.str_replace("\\$", "\\\\$", by);
		by = Global.str_replace("$$", "\\$", by);
		if (!Global.preg_match('/\\\\([^?].*?\\\\)/', re)) {
			by = Global.preg_replace('/\\$(\\d+)/', '\\$\\1', by);
		}
		var result = Global.preg_replace(reUnicode, by, s, global ? -1 : 1);
		if (result == null) {
			handlePregError();
			result = Global.preg_replace(re, by, s, global ? -1 : 1);
		}
		return result;
	}

	public function map(s:String, f:EReg->String):String {
		if(!matchFromByte(s, 0)) {
			return s;
		}
		var result = '';
		var bytesOffset = 0;
		var bytesTotal = Global.strlen(s);
		do {
			result += Global.substr(s, bytesOffset, matches[0][1] - bytesOffset);
			result += f(this);
			bytesOffset = matches[0][1];
			if(matches[0][0] == '') {
				result += Global.mb_substr(Global.substr(s, bytesOffset), 0, 1);
				bytesOffset++;
			} else {
				bytesOffset += Global.strlen(matches[0][0]);
			}
		} while(global && bytesOffset < bytesTotal && matchFromByte(s, bytesOffset));
		result += Global.substr(s, bytesOffset);
		return result;
	}

	public static inline function escape(s:String):String {
		return Global.preg_quote(s);
	}

	inline function get_reUnicode():String {
		return Syntax.concat(re, 'u');
	}
}
