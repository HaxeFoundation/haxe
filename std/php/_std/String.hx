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

@:coreApi extern class String {
	var length(default, null):Int;

	@:pure function new(string:String):Void;

	@:pure @:runtime inline function toUpperCase():String {
		return Global.mb_strtoupper(this);
	}

	@:pure @:runtime inline function toLowerCase():String {
		return Global.mb_strtolower(this);
	}

	@:pure @:runtime inline function charAt(index:Int):String {
		return index < 0 ? '' : Global.mb_substr(this, index, 1);
	}

	@:pure function charCodeAt(index:Int):Null<Int>;

	@:pure function indexOf(str:String, ?startIndex:Int):Int;

	@:pure function lastIndexOf(str:String, ?startIndex:Int):Int;

	@:pure function split(delimiter:String):Array<String>;

	@:pure @:runtime inline function substr(pos:Int, ?len:Int):String {
		return Global.mb_substr(this, pos, len);
	}

	@:pure function substring(startIndex:Int, ?endIndex:Int):String;

	@:pure @:runtime inline function toString():String {
		return this;
	}

	@:pure @:runtime static inline function fromCharCode(code:Int):String {
		return Global.mb_chr(code);
	}
}
