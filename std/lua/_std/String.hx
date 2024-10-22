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

import lua.Lua;
import lua.Table;
import lua.Boot;

#if lua_vanilla
private typedef BaseString = lua.NativeStringTools;
#else
private typedef BaseString = lua.lib.luautf8.Utf8;
#end

@:coreApi
class String {
	static var __oldindex:String->String->Dynamic;

	public var length(default, null):Int;

	public inline function new(string:String) untyped {}

	@:keep
	static function __index(s:Dynamic, k:Dynamic):Dynamic {
		if (k == "length")
			return BaseString.len(s);
		else if (Reflect.hasField(untyped String.prototype, k))
			return untyped String.prototype[k];
		else if (__oldindex != null) {
			if (Lua.type(__oldindex) == "function") {
				return __oldindex(s, k);
			} else if (Lua.type(__oldindex) == "table") {
				return untyped __oldindex[k];
			}
			return null;
		} else
			return null;
	}

	public inline function toUpperCase():String
		return BaseString.upper(this);

	public inline function toLowerCase():String
		return BaseString.lower(this);

	public function indexOf(str:String, ?startIndex:Int):Int {
		if (startIndex == null || startIndex < 0)
			startIndex = 1;
		else
			startIndex += 1;
		if (str == "") {
			return indexOfEmpty(this, startIndex - 1);
		}
		var r = BaseString.find(this, str, startIndex, true).begin;
		if (r != null && r > 0)
			return r - 1;
		else
			return -1;
	}

	static function indexOfEmpty(s:String, startIndex:Int):Int {
		var length = BaseString.len(s);
		if (startIndex < 0) {
			startIndex = 0;
		}
		return startIndex > length ? length : startIndex;
	}

	public function lastIndexOf(str:String, ?startIndex:Int):Int {
		var ret = -1;
		if (startIndex == null)
			startIndex = length;
		if (str == "") {
			if (this == "") {
				return 0;
			} else {
				var max = cast Math.max(startIndex, 0);
				return cast Math.min(length, max);
			}
		} else {
			while (true) {
				var p = indexOf(str, ret + 1);
				if (p == -1 || p > startIndex || p == ret)
					break;
				ret = p;
			}
			return ret;
		}
	}

	public function split(delimiter:String):Array<String> {
		var idx:Null<Int> = 1;
		var ret = [];
		while (idx != null) {
			var newidx:Null<Int> = 0;
			if (delimiter.length > 0) {
				newidx = BaseString.find(this, delimiter, idx, true).begin;
			} else if (idx >= this.length) {
				newidx = null;
			} else {
				newidx = idx + 1;
			}

			if (newidx != null) {
				var match = BaseString.sub(this, idx, newidx - 1).match;
				ret.push(match);
				idx = newidx + delimiter.length;
			} else {
				ret.push(BaseString.sub(this, idx, this.length).match);
				idx = null;
			}
		}
		return ret;
	}

	public inline function toString():String {
		return this;
	}

	public function substring(startIndex:Int, ?endIndex:Int):String {
		if (endIndex == null)
			endIndex = this.length;
		if (endIndex < 0)
			endIndex = 0;
		if (startIndex < 0)
			startIndex = 0;
		if (endIndex < startIndex) {
			// swap the index positions
			return BaseString.sub(this, endIndex + 1, startIndex).match;
		} else {
			return BaseString.sub(this, startIndex + 1, endIndex).match;
		}
	}

	public inline function charAt(index:Int):String {
		return BaseString.sub(this, index + 1, index + 1).match;
	}

	public inline function charCodeAt(index:Int):Null<Int> {
		return (BaseString.byte(this, index + 1) : Null<Int>);
	}

	public function substr(pos:Int, ?len:Int):String {
		if (len == null || len > pos + this.length)
			len = this.length;
		else if (len < 0)
			len = length + len;
		if (pos < 0)
			pos = length + pos;
		if (pos < 0)
			pos = 0;
		return BaseString.sub(this, pos + 1, pos + len).match;
	}

	public inline static function fromCharCode(code:Int):String {
		return BaseString.char(code);
	}
}
