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
@:forward
abstract Pipe(Stream) to Stream to Handle {
	/**
		Initialize a pipe handle.

		The `ipc` indicates if this pipe will be used for handle passing between
		processes (which may change the bytes on the wire). Only a connected pipe
		that will be passing the handles should have this flag set, not the
		listening pipe that `accept()` is called on.
	**/
	@:hlNative("uv", "pipe_init_wrap")
	static public function init(loop:Loop, ?ipc:Bool):Pipe
		return null;

	/**
		Bind the pipe to a file path (Unix) or a name (Windows).
	**/
	@:hlNative("uv", "pipe_bind_wrap")
	public function bind(name:String):Void {}

	/**
		Connect to the Unix domain socket or the named pipe.
	**/
	@:hlNative("uv", "pipe_connect_wrap")
	public function connect(name:String, callback:(e:UVError)->Void):Void {}

	/**
		Set the number of pending pipe instance handles when the pipe server is
		waiting for connections.

		This setting applies to Windows only.
	**/
	@:hlNative("uv", "pipe_pending_instances_wrap")
	public function pendingInstances(count:Int):Void {}

	/**
		Get the current address to which the handle is bound.
	**/
	public inline function getSockName():String
		return @:privateAccess String.fromUTF8(getsockname_wrap());

	@:hlNative("uv", "pipe_getsockname_wrap") function getsockname_wrap():Bytes
		return null;

	/**
		Get the address of the peer connected to the handle.
	**/
	public inline function getPeerName():String
		return @:privateAccess String.fromUTF8(getpeername_wrap());

	@:hlNative("uv", "pipe_getpeername_wrap") function getpeername_wrap():Bytes
		return null;

	/**
		Amount of handles waiting to be received.
	**/
	@:hlNative("uv", "pipe_pending_count_wrap")
	public function pendingCount():Int
		return 0;

	/**
		Used to receive handles over IPC pipes.

		First - call `pendingCount()`, if it’s > 0 then initialize a handle of the
		given type, returned by `pendingType()` and call `accept(handle)`.
	**/
	@:hlNative("uv", "pipe_pending_type_wrap") public function pendingType():HandleType
		return UV_UNKNOWN_HANDLE;

	/**
		This call is used in conjunction with `listen()` to accept incoming connections.
		Call this function after receiving a listen callback to accept the connection.

		Server(this stream) and `client` must be handles running on the same loop.

		@see http://docs.libuv.org/en/v1.x/stream.html#c.uv_accept
	**/
	@:hlNative("uv", "accept_wrap")
	public function accept(client:Stream):Void {}

	/**
		Extended write function for sending handles over a pipe.
		The pipe must be initialized with ipc == true.
	**/
	@:hlNative("uv", "write2_wrap")
	public function write2(bytes:Bytes, length:Int, sendHandle:Stream, callback:(e:UVError)->Void):Void {}
}
