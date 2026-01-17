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

package cs;

@:keep
class StringExt {
	public static function fromCharCode(code:Int):String {
		return untyped __cs__("((char){0}).ToString()", code);
	}

	public static function charAt(me:String, index:Int):String {
		if (index >= me.length || index < 0)
			return "";
		else
			return untyped __cs__("{0}[{1}].ToString()", me, index);
	}

	public static function charCodeAt(me:String, index:Int):Null<Int> {
		if (index >= me.length || index < 0)
			return null;
		else
			return untyped __cs__("(int){0}[{1}]", me, index);
	}

	// Fast unchecked charCodeAt - used by StringTools.fastCodeAt
	public static inline function cca(me:String, index:Int):Int {
		return untyped __cs__("(int){0}[{1}]", me, index);
	}

	public static function indexOf(me:String, str:String, startIndex:Null<Int>):Int {
		if (str.length == 0) {
			var si = startIndex == null ? 0 : startIndex;
			if (si < 0) si = 0;
			if (si > me.length) si = me.length;
			return si;
		}
		if (startIndex == null)
			return untyped __cs__("{0}.IndexOf({1})", me, str);
		else
			return untyped __cs__("{0}.IndexOf({1}, {2})", me, str, startIndex);
	}

	public static function lastIndexOf(me:String, str:String, ?startIndex:Int):Int {
		if (str.length == 0) {
			var si = startIndex == null ? me.length : startIndex;
			if (si < 0) si = 0;
			if (si > me.length) si = me.length;
			return si;
		}
		if (startIndex == null || startIndex > me.length - 1 || startIndex < 0) {
			startIndex = me.length - 1;
		}
		return untyped __cs__("{0}.LastIndexOf({1}, {2})", me, str, startIndex);
	}

	public static function split(me:String, delimiter:String):Array<String> {
		var ret = new Array<String>();
		if (delimiter.length == 0) {
			for (i in 0...me.length) {
				ret.push(charAt(me, i));
			}
		} else {
			// Split returns string[], wrap each element into the result array
			var nativeParts:cs.NativeArray<String> = untyped __cs__("{0}.Split(new string[] { {1} }, System.StringSplitOptions.None)", me, delimiter);
			var i = 0;
			var len:Int = untyped __cs__("{0}.Length", nativeParts);
			while (i < len) {
				ret.push(untyped __cs__("{0}[{1}]", nativeParts, i));
				i++;
			}
		}
		return ret;
	}

	public static function substr(me:String, pos:Int, ?len:Int):String {
		var length:Int = len == null ? me.length : len;
		if (pos < 0) {
			pos = me.length + pos;
			if (pos < 0) {
				pos = 0;
			}
		}
		if (length < 0) {
			length = me.length + length - pos;
		}
		if (pos + length > me.length) {
			length = me.length - pos;
		}
		if (pos < 0 || length <= 0) {
			return "";
		}
		return untyped __cs__("{0}.Substring({1}, {2})", me, pos, length);
	}

	public static function substring(me:String, startIndex:Int, ?endIndex:Int):String {
		var end:Int = endIndex == null ? me.length : endIndex;
		if (end < 0) {
			end = 0;
		} else if (end > me.length) {
			end = me.length;
		}
		if (startIndex < 0) {
			startIndex = 0;
		} else if (startIndex > me.length) {
			startIndex = me.length;
		}

		if (startIndex > end) {
			var tmp = startIndex;
			startIndex = end;
			end = tmp;
		}
		return untyped __cs__("{0}.Substring({1}, {2})", me, startIndex, end - startIndex);
	}
}
