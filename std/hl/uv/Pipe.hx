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

import hl.uv.SockAddr;
import hl.uv.Handle;

/**
	Handles, which can be received via `hl.uv.Pipe.receiveHandle()`
**/
enum ReceiveHandle {
	NONE;
	TCP(associate:(tcp:Tcp)->Void);
	PIPE(associate:(pipe:Pipe)->Void);
}

/**
	Pipe handles provide an abstraction over streaming files on Unix (including local domain sockets, pipes, and FIFOs) and named pipes on Windows.

	@see http://docs.libuv.org/en/v1.x/pipe.html
**/
class Pipe extends Stream<UvPipeTStar> {
	/**
		Initialize a pipe handle.

		The `ipc` indicates if this pipe will be used for handle passing between
		processes (which may change the bytes on the wire). Only a connected pipe
		that will be passing the handles should have this flag set, not the
		listening pipe that `accept()` is called on.
	**/
	static public function init(loop:Loop, ipc:Bool = false):Pipe {
		loop.checkLoop();
		var pipe = new Pipe(UV.alloc_pipe());
		var result = loop.pipe_init(pipe.h, ipc ? 1 : 0);
		if(result < 0) {
			pipe.freeHandle();
			result.throwErr();
		}
		return pipe;
	}

	/**
		Bind the pipe to a file path (Unix) or a name (Windows).
	**/
	public function bind(name:String):Void {
		handle(h -> h.pipe_bind(name.toUTF8()).resolve());
	}

	/**
		Connect to the Unix domain socket or the named pipe.
	**/
	public function connect(name:String, callback:(e:UVError)->Void):Void {
		handle(h -> {
			var req = Stream.createConnect();
			req.r.pipe_connect_with_cb(h, name.toUTF8());
			req.callback = status -> {
				req.freeReq();
				callback(status.translate_uv_error());
			}
		});
	}

	/**
		Set the number of pending pipe instance handles when the pipe server is
		waiting for connections.

		This setting applies to Windows only.
	**/
	public function pendingInstances(count:Int):Void {
		handle(h -> h.pipe_pending_instances(count));
	}

	/**
		Get the current address to which the handle is bound.
	**/
	public inline function getSockName():String {
		return handleReturn(h -> getName(h, false));
	}

	/**
		Get the address of the peer connected to the handle.
	**/
	public inline function getPeerName():String {
		return handleReturn(h -> getName(h, true));
	}

	static inline function getName(h:UvPipeTStar, getPeer:Bool) {
		var size = I64.ofInt(256);
		var buf = null;
		var eNoBufs = UVError.UV_ENOBUFS.toNative();
		var result = eNoBufs;
		while (result == eNoBufs) {
			buf = new Bytes(size.toInt());
			result = getPeer ? h.pipe_getpeername(buf,Ref.make(size)) : h.pipe_getsockname(buf,Ref.make(size));
		}
		result.resolve();
		return buf.fromUTF8();
	}

	/**
		Amount of handles waiting to be received.
	**/
	public function pendingCount():Int {
		return handleReturn(h -> h.pipe_pending_count());
	}

	/**
		Used to receive handles over IPC pipes.

		First - call `pendingCount()`, if it’s > 0 then initialize a handle of the
		given type, returned by `pendingType()` and call `accept(handle)`.
	**/
	public function pendingType():HandleType {
		return handleReturn(h -> h.pipe_pending_type());
	}

	/**
		This call is used in conjunction with `listen()` to accept incoming connections.
		Call this function after receiving a listen callback to accept the connection.

		Server(this stream) and `client` must be handles running on the same loop.

		@see http://docs.libuv.org/en/v1.x/stream.html#c.uv_accept
	**/
	public function accept<T:UvStreamTStar>(client:Stream<T>):Void {
		handle(server -> client.handle(client -> {
			server.accept(client).resolve();
		}));
	}

	/**
		Extended write function for sending handles over a pipe.
		The pipe must be initialized with ipc == true.
	**/
	public function write2<T:UvStreamTStar>(bytes:Bytes, length:Int, sendHandle:Stream<T>, callback:(e:UVError)->Void):Void {
		handle(h -> sendHandle.handle(sendHandle -> {
			var req = Stream.createWrite();
			var buf = UV.alloc_buf(bytes, length); // TODO: need to free buf manually?
			var result = req.r.write2_with_cb(h, buf, 1, sendHandle);
			if(result < 0) {
				req.freeReq();
				result.throwErr();
			}
			req.data = bytes;
			req.callback = status -> {
				req.freeReq();
				callback(status.translate_uv_error());
			}
		}));
	}
}
