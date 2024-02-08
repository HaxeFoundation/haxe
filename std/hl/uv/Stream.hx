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

package hl.uv;

@:hlNative("uv")
class Stream extends Handle {
	public function write(bytes:haxe.io.Bytes, ?onWrite:Bool->Void, pos = 0, len = -1) {
		if (len < 0)
			len = bytes.length - pos;
		if (pos < 0 || len < 0 || pos + len > bytes.length)
			throw haxe.io.Error.OutsideBounds;
		if (handle == null || !stream_write(handle, (bytes : hl.Bytes).offset(pos), len, onWrite))
			throw new haxe.io.Eof();
	}

	public function readStartRaw(onData:hl.Bytes->Int->Void) {
		if (handle == null || !stream_read_start(handle, onData))
			throw new haxe.io.Eof();
	}

	public function readStart(onData:haxe.io.Bytes->Void) {
		readStartRaw(function(b, len) onData(if (len < 0) null else b.toBytes(len)));
	}

	public function readStop() {
		if (handle != null)
			stream_read_stop(handle);
	}

	public function listen(n:Int, onConnect:Void->Void) {
		if (handle == null || !stream_listen(handle, n, onConnect))
			throw new haxe.io.Eof();
	}

	// --

	static function stream_write(handle:HandleData, bytes:hl.Bytes, len:Int, callb:Bool->Void):Bool {
		return false;
	}

	static function stream_read_start(handle:HandleData, callb:hl.Bytes->Int->Void) {
		return false;
	}

	static function stream_read_stop(handle:HandleData) {}

	static function stream_listen(handle:HandleData, n:Int, callb:Void->Void) {
		return false;
	}
}
