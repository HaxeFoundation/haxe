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
import haxe.io.Eof;
import haxe.io.Input;
import cs.system.io.Stream;

class FileInput extends Input {
	var stream:Stream;
	var _eof:Bool;

	@:allow(sys.io.File)
	function new(stream:Stream) {
		this.stream = stream;
		this._eof = false;
	}

	override public function close() {
		try {
			stream.Close();
		} catch (e:Dynamic) {
			throw e;
		}
	}

	override public function readByte():Int {
		var b:Int = stream.ReadByte();
		if (b == -1) {
			_eof = true;
			throw new Eof();
		}
		return b;
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var ret:Int = cs.Syntax.code("{0}.Read({1}, {2}, {3})", stream, s.getData(), pos, len);
		if (ret == 0) {
			_eof = true;
			throw new Eof();
		}
		return ret;
	}

	public function seek(p:Int, pos:FileSeek):Void {
		_eof = false;
		// Map Haxe FileSeek enum to C# SeekOrigin enum
		var origin:Int = switch (pos) {
			case SeekBegin: 0; // SeekOrigin.Begin
			case SeekCur: 1; // SeekOrigin.Current
			case SeekEnd: 2; // SeekOrigin.End
		};
		cs.Syntax.code("{0}.Seek({1}, (System.IO.SeekOrigin){2})", stream, p, origin);
	}

	public function tell():Int {
		return cs.Syntax.code("(int){0}.Position", stream);
	}

	public inline function eof():Bool {
		return _eof;
	}
}
