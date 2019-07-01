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

package python.io;

import haxe.io.Output;
import python.Bytearray;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeBytesOutput extends NativeOutput<RawIOBase> {
	public function new(stream:RawIOBase) {
		super(stream);
	}

	public function seek(p:Int, pos:sys.io.FileSeek):Void {
		return IoTools.seekInBinaryMode(stream, p, pos);
	}

	override public function prepare(nbytes:Int):Void {
		stream.truncate(nbytes);
	}

	override public function writeByte(c:Int):Void {
		stream.write(new Bytearray([c]));
	}

	override function writeBytes(s:haxe.io.Bytes, pos:Int, len:Int):Int {
		return stream.write(python.Syntax.code("{0}[{1}:{2}]", s.getData(), pos, pos + len));
	}
}
