/*
 * Copyright (C)2005-2016 Haxe Foundation
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
import lua.Io;
import lua.NativeStringTools;
import lua.Boot;
import lua.Os;

class FileInput extends haxe.io.Input {
	var f:FileHandle;

	public function new(f:FileHandle){
		this.bigEndian = Boot.platformBigEndian;
		this.f = f;
	}

	inline public function seek( p : Int, pos : FileSeek ) : Void {
		var arg = switch(pos){
			case SeekBegin : "set";
			case SeekCur   : "cur";
			case SeekEnd   : "end";
		}
		return f.seek(arg, p);
	}

	inline public function tell() : Int {
		return f.seek();
	}

	inline public function eof() : Bool {
		return f.read(0) == null;
	}

	override inline public function readByte() : Int {
		return NativeStringTools.byte(f.read(1));
	}

	override inline public function close() : Void {
		f.close();
	}

}
