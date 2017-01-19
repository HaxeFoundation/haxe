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

import haxe.io.Eof;
import haxe.io.Input;
import python.Bytearray;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeInput<T:IOBase> extends Input {

	var stream:T;
	var wasEof:Bool;

	function new (s:T) {
		this.stream = s;
		this.bigEndian = false;
		wasEof = false;
		if (!stream.readable()) throw "Write-only stream";
	}

	public var canSeek(get,never):Bool;
	inline function get_canSeek():Bool return stream.seekable();

	override public function close():Void
	{
		stream.close();
	}

	public function tell() : Int
	{
		return stream.tell();
	}

	function throwEof() {
		wasEof = true;
		throw new Eof();
	}

	public function eof() {
		return wasEof;
	}

	function readinto (b:Bytearray):Int {
		throw "abstract method, should be overriden";
	}

	function seek (p:Int, mode:sys.io.FileSeek) {
		throw "abstract method, should be overriden";
	}

	override public function readBytes(s:haxe.io.Bytes, pos:Int, len:Int):Int {
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;

		var ba = new Bytearray(len);
		var ret = readinto(ba);
		if (ret == 0)
			throwEof();
		s.blit(pos, haxe.io.Bytes.ofData(ba), 0, len);
		return ret;
	}
}