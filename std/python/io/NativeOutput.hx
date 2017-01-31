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

import haxe.io.Output;

import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeOutput<T:IOBase> extends Output {

	var stream:T;

	public var canSeek(get,never):Bool;
	inline function get_canSeek():Bool return stream.seekable();

	public function new (stream:T) {
		this.bigEndian = false;
		this.stream = stream;
		if (!stream.writable()) throw "Read only stream";
	}

	override public function close():Void
	{
		stream.close();
	}

	override public function prepare(nbytes:Int):Void
	{
		stream.truncate(nbytes);
	}

	override public function flush():Void
	{
		stream.flush();
	}


	public function tell() : Int
	{
		return stream.tell();
	}
}