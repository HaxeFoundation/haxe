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

package sys.io;

import haxe.io.Bytes;
import haxe.io.Output;

class FileOutput extends Output {
	var stream:Dynamic; // System.IO.FileStream

	@:allow(sys.io.File)
	function new(stream:Dynamic) {
		this.stream = stream;
	}

	override public function close() {
		try {
			untyped __cs__("{0}.Close()", stream);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	override public function writeByte(c:Int):Void {
		untyped __cs__("{0}.WriteByte((byte){1})", stream, c);
	}

	override public function write(s:Bytes):Void {
		untyped __cs__("{0}.Write({1}, 0, {2})", stream, s.getData(), s.length);
	}

	override public function writeBytes(s:Bytes, pos:Int, len:Int):Int {
		untyped __cs__("{0}.Write({1}, {2}, {3})", stream, s.getData(), pos, len);
		return len;
	}

	public function seek(p:Int, pos:FileSeek):Void {
		var origin:Int = switch (pos) {
			case SeekBegin: 0; // System.IO.SeekOrigin.Begin
			case SeekCur: 1; // System.IO.SeekOrigin.Current
			case SeekEnd: 2; // System.IO.SeekOrigin.End
		};
		untyped __cs__("{0}.Seek({1}, (System.IO.SeekOrigin){2})", stream, p, origin);
	}

	public function tell():Int {
		return untyped __cs__("(int){0}.Position", stream);
	}

	override public function flush():Void {
		untyped __cs__("{0}.Flush()", stream);
	}
}
