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

package python.lib.io;

enum abstract SeekSet(Int) {
	var SeekSet = 0;
	var SeekCur = 1;
	var SeekEnd = 2;
}

@:pythonImport("io", "IOBase")
extern class IOBase implements IIOBase {
	function close():Void;
	function flush():Void;
	function readline(limit:Int = -1):String;
	function readable():Bool;
	var closed(default, null):Bool;
	function readlines(hint:Int = -1):Array<String>;
	function tell():Int;
	function writable():Bool;
	function seekable():Bool;
	function fileno():Int;
	function seek(offset:Int, whence:SeekSet):Int;
	function truncate(size:Int):Int;
}

@:remove extern interface IIOBase {
	function close():Void;
	function flush():Void;
	function readline(limit:Int = -1):String;
	function readable():Bool;
	var closed(default, null):Bool;
	function readlines(hint:Int = -1):Array<String>;
	function tell():Int;
	function writable():Bool;
	function seekable():Bool;
	function fileno():Int;
	function seek(offset:Int, whence:SeekSet):Int;
	function truncate(size:Int):Int;
}
