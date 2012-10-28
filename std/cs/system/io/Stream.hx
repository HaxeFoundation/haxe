/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package cs.system.io;
import cs.StdTypes;
import haxe.Int64;
import haxe.io.BytesData;

@:native('System.IO.Stream') extern class Stream 
{
	var CanRead(default, null):Bool;
	var CanSeek(default, null):Bool;
	var CanTimeout(default, null):Bool;
	var CanWrite(default, null):Bool;
	var Length(default, null):Int64;
	var Position(default, null):Int64;
	var ReadTimeout:Bool;
	var WriteTimeout:Bool;
	
	function Close():Void;
	function CopyTo(dest:Stream):Void;
	function Dispose():Void;
	function Flush():Void;
	function Read(buf:BytesData, offset:Int, count:Int):Int;
	function ReadByte():Int;
	function Seek(offset:Int64, origin:SeekOrigin):Int64;
	function SetLength(value:Int64):Void;
	function Write(buf:BytesData, offset:Int, count:Int):Void;
	function WriteByte(value:UInt8):Void;
}