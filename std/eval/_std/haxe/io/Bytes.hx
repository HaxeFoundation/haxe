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

package haxe.io;

// @:coreApi
extern class Bytes {
	function new(length:Int, b:BytesData):Void;
	var length(default, null):Int;
	function get(pos:Int):Int;
	function set(pos:Int, v:Int):Void;
	function blit(pos:Int, src:Bytes, srcpos:Int, len:Int):Void;
	function fill(pos:Int, len:Int, value:Int):Void;
	function sub(pos:Int, len:Int):Bytes;
	function compare(other:Bytes):Int;
	function getDouble(pos:Int):Float;
	function getFloat(pos:Int):Float;
	function setDouble(pos:Int, v:Float):Void;
	function setFloat(pos:Int, v:Float):Void;
	function getUInt16(pos:Int):Int;
	function setUInt16(pos:Int, v:Int):Void;
	function getInt32(pos:Int):Int;
	function getInt64(pos:Int):haxe.Int64;
	function setInt32(pos:Int, v:Int):Void;
	function setInt64(pos:Int, v:haxe.Int64):Void;
	function getString(pos:Int, len:Int, ?encoding:Encoding):String;
	function toString():String;
	function toHex():String;
	function getData():BytesData;
	static function alloc(length:Int):Bytes;
	@:pure
	static function ofString(s:String, ?encoding:Encoding):Bytes;
	static function ofData(b:BytesData):Bytes;
	static function ofHex(s:String):Bytes;
	static function fastGet(b:BytesData, pos:Int):Int;
	static function __init__():Void {
		haxe.io.Error;
	}
}
