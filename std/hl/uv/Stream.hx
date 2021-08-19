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

import hl.uv.Request;

private class ConnectData extends RequestData {
	public final callback:(e:UVError)->Void;

	public function new(callback) {
		this.callback = callback;
	}
}

abstract ConnectRequest(Request) to Request {}

private class ShutdownData extends RequestData {
	public final callback:(e:UVError)->Void;

	public function new(callback) {
		this.callback = callback;
	}
}

abstract ShutdownRequest(Request) to Request {}

private class WriteData extends RequestData {
	public final callback:(e:UVError)->Void;

	public function new(callback) {
		this.callback = callback;
	}
}

abstract WriteRequest(Request) to Request {}

/**
	Stream handles provide an abstraction of a duplex communication channel.
	This is a base type for `Tcp`, `Pipe` and `Tty`.

	@see http://docs.libuv.org/en/v1.x/stream.html
**/
@:forward
abstract Stream(Handle) to Handle {
	/**
		Shutdown the outgoing (write) side of a duplex stream.
		It waits for pending write requests to complete.
	**/
	@:hlNative("uv", "shutdown_wrap")
	public function shutdown(callback:(e:UVError)->Void):Void {}

	/**
		Start listening for incoming connections.
		`backlog` indicates the number of connections the kernel might queue
	**/
	@:hlNative("uv", "listen_wrap")
	public function listen(backlog:Int, callback:(e:UVError)->Void):Void {}

	/**
		Read data from an incoming stream.

		The `callback` will be called several times until there is no more
		data to read or `readStop()` is called. If `bytesRead` is 0 it does not
		indicate EOF. It means either there's no data to read _right now_ or IO
		operation would have to block.
	**/
	@:hlNative("uv", "read_start_wrap")
	public function readStart(callback:(e:UVError, data:Bytes, bytesRead:Int)->Void):Void {}

	/**
		Stop reading data from the stream.
	**/
	@:hlNative("uv", "read_stop_wrap")
	public function readStop():Void {}

	/**
		Write data to stream.
	**/
	@:hlNative("uv", "write_wrap")
	public function write(bytes:hl.Bytes, length:Int, callback:(e:UVError)->Void):Void {}

	/**
		Same as `write()`, but won’t queue a write request if it can’t be completed immediately.

		Returns number of bytes writen (can be less than the supplied buffer size).

		Throws EAGAIN if no data can be sent immediately
	**/
	@:hlNative("uv", "try_write_wrap")
	public function try_write(bytes:hl.Bytes, length:Int):Int
		return 0;

	/**
		Indicates if the stream is readable.
	**/
	@:hlNative("uv", "is_readable_wrap")
	public function isReadable():Bool
		return false;

	/**
		Indicates if the stream is writable.
	**/
	@:hlNative("uv", "is_writable_wrap")
	public function isWritable():Bool
		return false;

}
