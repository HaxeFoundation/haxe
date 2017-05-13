/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package python.io;

import haxe.io.Bytes;

interface IInput
{
	public var bigEndian(default,set) : Bool;

	public function readByte() : Int;

	public function readBytes( s : Bytes, pos : Int, len : Int ) : Int;

	public function close():Void;

	public function readAll( ?bufsize : Int ) : Bytes;

	public function readFullBytes( s : Bytes, pos : Int, len : Int ):Void;

	public function read( nbytes : Int ) : Bytes;

	public function readUntil( end : Int ) : String;

	public function readLine() : String;

	public function readFloat() : Float;

	public function readDouble() : Float;

	public function readInt8():Int;

	public function readInt16():Int;

	public function readUInt16():Int;

	public function readInt24():Int;

	public function readUInt24():Int;

	public function readInt32():Int;

	public function readString( len : Int ) : String;
}