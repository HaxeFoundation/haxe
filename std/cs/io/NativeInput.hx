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

package cs.io;

import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Input;
import cs.system.io.Stream;

class NativeInput extends Input {
	public var stream:Stream;

	public function new(stream:Stream) {
		this.stream = stream;
	}

	override public function readByte():Int {
		var ret:Int = 0;
		try {
			ret = stream.ReadByte();
		} catch (e:Dynamic) {
			throw haxe.io.Error.Custom(e);
		}
		if (ret == -1)
			throw new Eof();
		return ret;
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || pos + len > s.length)
			throw haxe.io.Error.OutsideBounds;
		var ret:Int = 0;
		try {
			ret = untyped __cs__("{0}.Read({1}, {2}, {3})", stream, s.getData(), pos, len);
		} catch (e:Dynamic) {
			throw haxe.io.Error.Custom(e);
		}
		if (ret == 0)
			throw new Eof();
		return ret;
	}

	override public function close():Void {
		try {
			stream.Close();
		} catch (e:Dynamic) {
			throw haxe.io.Error.Custom(e);
		}
	}
}
