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
@:native("java.lang.String")
extern class String implements java.lang.CharSequence {
	var length(default, null):Int;

	@:overload(function(b:haxe.io.BytesData, offset:Int, length:Int, charsetName:String):Void {})
	@:overload(function(b:haxe.io.BytesData, offset:Int, length:Int):Void {})
	@:overload(function(b:java.NativeArray<java.StdTypes.Char16>):Void {})
	@:overload(function(b:java.NativeArray<Int>, offset:Int, count:Int):Void {})
	function new(string:String):Void;

	function toUpperCase():String;
	function toLowerCase():String;

	@:runtime inline function charAt(index:Int):String {
		return jvm.StringExt.charAt(this, index);
	}

	inline function charCodeAt(index:Int):Null<Int> {
		return jvm.StringExt.charCodeAt(this, index);
	}

	inline function indexOf(str:String, ?startIndex:Int):Int {
		return jvm.StringExt.indexOf(this, str, startIndex);
	}

	@:runtime inline function lastIndexOf(str:String, ?startIndex:Int):Int {
		return jvm.StringExt.lastIndexOf(this, str, startIndex);
	}

	@:runtime inline function split(delimiter:String):Array<String> {
		return jvm.StringExt.split(this, delimiter);
	}

	@:runtime inline function substr(pos:Int, ?len:Int):String {
		return jvm.StringExt.substr(this, pos, len);
	}

	@:runtime inline function substring(startIndex:Int, ?endIndex:Int):String {
		return jvm.StringExt.substring(this, startIndex, endIndex);
	}

	function toString():String;

	private function compareTo(anotherString:String):Int;

	private function codePointAt(idx:Int):Int;

	@:overload(function():haxe.io.BytesData {})
	private function getBytes(encoding:String):haxe.io.BytesData;

	@:runtime static inline function fromCharCode(code:Int):String {
		return jvm.StringExt.fromCharCode(code);
	}
}
