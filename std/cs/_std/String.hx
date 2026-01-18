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

@:coreApi
@:native("string")
extern class String {
	var length(default, null):Int;

	function new(string:String):Void;

	@:native("ToUpper") function toUpperCase():String;
	@:native("ToLower") function toLowerCase():String;

	@:runtime inline function charAt(index:Int):String {
		return cs.StringExt.charAt(this, index);
	}

	inline function charCodeAt(index:Int):Null<Int> {
		return cs.StringExt.charCodeAt(this, index);
	}

	inline function indexOf(str:String, ?startIndex:Int):Int {
		return cs.StringExt.indexOf(this, str, startIndex);
	}

	@:runtime inline function lastIndexOf(str:String, ?startIndex:Int):Int {
		return cs.StringExt.lastIndexOf(this, str, startIndex);
	}

	@:runtime inline function split(delimiter:String):Array<String> {
		return cs.StringExt.split(this, delimiter);
	}

	@:runtime inline function substr(pos:Int, ?len:Int):String {
		return cs.StringExt.substr(this, pos, len);
	}

	@:runtime inline function substring(startIndex:Int, ?endIndex:Int):String {
		return cs.StringExt.substring(this, startIndex, endIndex);
	}

	@:native("ToString") function toString():String;

	@:runtime static inline function fromCharCode(code:Int):String {
		return cs.StringExt.fromCharCode(code);
	}
}
