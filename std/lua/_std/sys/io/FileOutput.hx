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

import lua.FileHandle;
import haxe.io.Bytes;

class FileOutput extends haxe.io.Output {
	var f:FileHandle;

	function new(f:FileHandle) {
		if (f == null)
			throw 'Invalid filehandle : $f';
		this.f = f;
	}

	public inline function seek(p:sys.FilePos, pos:FileSeek):Void {
		var arg = switch (pos) {
			case SeekBegin: "set";
			case SeekCur: "cur";
			case SeekEnd: "end";
		}
		var r:Null<Float> = f.seek(arg, cast p.toFloat());
		if (r == null)
			throw haxe.io.Error.Custom("seek() failure");
	}

	public inline function tell():sys.FilePos {
		var p:Null<Float> = f.seek();
		if (p == null || p < 0)
			throw haxe.io.Error.Custom("tell() failure");
		return sys.FilePos.fromFloat(p);
	}

	override inline public function writeByte(c:Int):Void {
		f.write(String.fromCharCode(c));
	}

	override inline public function writeBytes(s:Bytes, pos:Int, len:Int):Int {
		f.write(s.getString(pos, len));
		return s.length;
	}

	override public function close() {
		f.close();
	}
}
