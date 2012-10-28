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
package java.io;
import haxe.Int64;
import haxe.io.BytesData;
import java.StdTypes;

//FIXME: this is incomplete
extern class RandomAccessFile 
{
	function new(f:File, mode:String):Void;
	
	function close():Void;
	function getFilePointer():Int64;
	function length():Int64;
	@:overload(function(b:BytesData, pos:Int, len:Int):Int {})
	function read():Int;
	
	function readBoolean():Bool;
	function readByte():Int8;
	function readChar():Char16;
	function readDouble():Float;
	function readFloat():Single;
	@:overload(function(b:BytesData, off:Int, len:Int):Void {})
	function readFully(b:BytesData):Void;
	function readInt():Int;
	function readLine():String;
	function readLong():Int64;
	function readShort():Int16;
	function readUnsignedByte():Int;
	function readUnsignedShort():Int;
	function readUTF():String;
	
	function seek(pos:Int64):Void;
	function setLength(newLength:Int):Void;
	function skipBytes(n:Int):Int;
	
	@:overload(function(b:BytesData, off:Int, len:Int):Void {})
	@:overload(function(b:BytesData):Void {})
	function write(i:Int):Void;
	
	function writeBoolean(b:Bool):Void;
	function writeByte(v:Int):Void;
	function writeChar(v:Int):Void;
	function writeChars(s:String):Void;
	function writeDouble(v:Float):Void;
	function writeFloat(v:Single):Void;
	function writeInt(v:Int):Void;
	function writeLong(v:Int64):Void;
	function writeShort(v:Int16):Void;
	function writeUTF(str:String):Void;
}