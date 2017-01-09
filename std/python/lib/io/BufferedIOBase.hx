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
package python.lib.io;

import python.lib.io.IOBase;

import python.lib.io.RawIOBase;
import python.Bytearray;

@:pythonImport("io", "BufferedIOBase")
extern class BufferedIOBase extends IOBase implements IBufferedIOBase {

	/* not always available */
	public var raw:RawIOBase;

	public function write (b:Bytearray):Int;
	public function readinto (b:Bytearray):Int;
	public function detach ():RawIOBase;
	public function read(n:Int = -1):Null<Bytes>;
	public function read1(n:Int = -1):Null<Bytes>;
}


@:remove extern interface IBufferedIOBase extends IIOBase {
	public var raw:RawIOBase;

	public function write (b:Bytearray):Int;
	public function readinto (b:Bytearray):Int;
	public function detach ():RawIOBase;
	public function read(n:Int = -1):Null<Bytes>;
	public function read1(n:Int = -1):Null<Bytes>;
}