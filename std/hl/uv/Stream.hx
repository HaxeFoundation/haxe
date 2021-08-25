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

@:allow(hl.uv.Stream)
private class ConnectRequest extends Request<UvConnectTStar> {
	var callback:(status:Int)->Void;
}

@:allow(hl.uv.Stream)
private class WriteRequest extends Request<UvWriteTStar> {
	var callback:(status:Int)->Void;
	//to keep bytes alive untile write request is complete
	var data:Bytes;
}

@:allow(hl.uv.Stream)
private class ShutdownRequest extends Request<UvShutdownTStar> {
	var callback:(status:Int)->Void;
}


/**
	Stream handles provide an abstraction of a duplex communication channel.
	This is a base type for `Tcp`, `Pipe` and `Tty`.

	@see http://docs.libuv.org/en/v1.x/stream.html
**/
abstract class Stream<T:UvStreamTStar> extends Handle<T> {
	var onConnection:(status:Int)->Void;
	var onRead:(nRead:I64, buf:UvBufTArr)->Void;

	static inline function createConnect():ConnectRequest {
		return new ConnectRequest(UV.alloc_connect());
	}

	/**
		Shutdown the outgoing (write) side of a duplex stream.
		It waits for pending write requests to complete.
	**/
	public function shutdown(callback:(e:UVError)->Void):Void {
		handle(h -> {
			var req = new ShutdownRequest(UV.alloc_shutdown());
			var result = req.r.shutdown_with_cb(h);
			if(result < 0) {
				req.freeReq();
				result.throwErr();
			}
			req.callback = status -> {
				req.freeReq();
				callback(status.translate_uv_error());
			}
		});
	}

	/**
		Start listening for incoming connections.
		`backlog` indicates the number of connections the kernel might queue
	**/
	public function listen(backlog:Int, callback:(e:UVError)->Void):Void {
		handle(h -> {
			h.listen_with_cb(backlog).resolve();
			onConnection = status -> callback(status.translate_uv_error());
		});
	}

	/**
		Read data from an incoming stream.

		The `callback` will be called several times until there is no more
		data to read or `readStop()` is called. If `bytesRead` is 0 it does not
		indicate EOF. It means either there's no data to read _right now_ or IO
		operation would have to block.
	**/
	public function readStart(callback:(e:UVError, data:Null<Bytes>, bytesRead:Int)->Void):Void {
		handle(h -> {
			h.read_start_with_cb().resolve();
			onRead = (nRead, buf) -> {
				var bytesRead = nRead.toInt();
				var e = bytesRead.translate_uv_error();
				var data = switch e {
					case UV_NOERR:
						@:privateAccess Bytes.copy(buf.buf_base(), bytesRead); // TODO: avoid copying bytes, but make sure it's automatically freed by GC
					case _:
						bytesRead = 0;
						null;
				}
				// if(buf != null) {
				// 	var base = buf.buf_base();
				// 	if(base != null)
				// 		base.bytes_to_pointer().free();
				// 	buf.buf_to_pointer().free();
				// }
				callback(e, data, bytesRead);
			}
		});
	}

	/**
		Stop reading data from the stream.
	**/
	public function readStop():Void {
		handle(h -> h.read_stop().resolve());
	}

	/**
		Write data to stream.
	**/
	public function write(bytes:hl.Bytes, length:Int, callback:(e:UVError)->Void):Void {
		handle(h -> {
			var req = new WriteRequest(UV.alloc_write());
			var buf = UV.alloc_buf(bytes, length); // TODO: need to free buf manually?
			// trace({length:length, buf_len:buf.buf_len()});
			var result = req.r.write_with_cb(h, buf, 1);
			if(result < 0) {
				req.freeReq();
				result.throwErr();
			}
			req.data = bytes;
			req.callback = status -> {
				req.freeReq();
				callback(status.translate_uv_error());
			}
		});
	}

	/**
		Same as `write()`, but won’t queue a write request if it can’t be completed immediately.

		Returns number of bytes writen (can be less than the supplied buffer size).

		Throws EAGAIN if no data can be sent immediately
	**/
	public function tryWrite(bytes:hl.Bytes, length:Int):Int {
		return handleReturn(h -> {
			var buf = UV.alloc_buf(bytes, length); // TODO: need to free buf manually?
			var result = h.try_write(buf, 1);
			if(result < 0)
				result.throwErr();
			return result;
		});
	}

	/**
		Indicates if the stream is readable.
	**/
	public function isReadable():Bool {
		return handleReturn(h -> h.is_readable() != 0);
	}

	/**
		Indicates if the stream is writable.
	**/
	public function isWritable():Bool {
		return handleReturn(h -> h.is_writable() != 0);
	}

}
