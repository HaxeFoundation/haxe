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

package cpp.uv;

import haxe.io.Bytes;
import cpp.uv.Handle;
import cpp.uv.Stream;

using cpp.uv.UV;

enum abstract PipeMode(Int) to Int {
	var READ = 0;
	var WRITE = 1;
	var READ_WRITE = 2;
}

/**
	Pipe handles provide an abstraction over streaming files on Unix (including
	local domain sockets, pipes, and FIFOs) and named pipes on Windows.

	@see http://docs.libuv.org/en/v1.x/pipe.html
**/
@:headerCode('#include "uv.h"')
class Pipe extends Stream {
	var uvPipe(get,never):RawPointer<UvPipeT>;

	inline function get_uvPipe():RawPointer<UvPipeT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvPipeT.create();
		super.setupUvData();
	}

	/**
		Initialize a pipe handle.

		The `ipc` argument indicates if this pipe will be used for handle passing
		between processes (which may change the bytes on the wire). Only a connected
		pipe that will be passing the handles should have this flag set, not the
		listening pipe that uv_accept is called on.
	**/
	static public function init(loop:Loop, ipc:Bool = false):Pipe {
		var pipe = new Pipe();
		UV.pipe_init(loop.uvLoop, pipe.uvPipe, ipc ? 1 : 0).resolve();
		return pipe;
	}

	/**
		Bind the pipe to a file path (Unix) or a name (Windows).
	**/
	public function bind(name:String) {
		UV.pipe_bind(uvPipe, name).resolve();
	}

	/**
		Connect to the Unix domain socket or the named pipe.
	**/
	public function connect(name:String, callback:(e:UVError)->Void) {
		var req = new ConnectRequest();
		UV.pipe_connect(req.uvConnect, uvPipe, name, Callable.fromStaticFunction(uvConnectCb));
		req.onConnect = callback;
	}

	static function uvConnectCb(uvConnect:RawPointer<UvConnectT>, status:Int) {
		var req:ConnectRequest = cast Request.get(cast uvConnect);
		req.onConnect(status.explain());
	}

	/**
		This call is used in conjunction with `pipe.listen(backlog, callback)` to
		accept incoming connections.
		Call this function after receiving a `callback` to accept the connection.

		This call is also used in conjunction with `pipe.pendingCount()` and `pipe.pendingType()`
		to accept incoming handles while reading from the pipe.
	**/
	public function accept(client:Stream) {
		UV.accept(uvStream, client.uvStream).resolve();
	}

	/**
		Extended write function for sending handles over a pipe.
		The pipe must be initialized with `ipc == true`.
	**/
	public function write2(data:Bytes, pos:UInt, length:UInt, sendHandle:Stream, callback:(e:UVError)->Void) {
		writeImpl(data, pos, length, callback, (r, h, b, cb) -> UV.write2(r, h, b, 1, sendHandle.uvStream, cb));
	}

	/**
		Same as `pipe.tryWrite` and extended write function for sending handles over a pipe.
	**/
	public function tryWrite2(data:Bytes, pos:UInt, length:UInt, sendHandle:Stream):Int {
		return tryWriteImpl(data, pos, length, (h, b) -> UV.try_write2(h, b, 1, sendHandle.uvStream));
	}

	/**
		Get the current address to which the handle is bound.
	**/
	public function getSockName() {
		return UV.getName((buf, size) -> UV.pipe_getsockname(uvPipe, buf, size));
	}

	/**
		Get the address of the peer connected to the handle.
	**/
	public function getPeerName() {
		return UV.getName((buf, size) -> UV.pipe_getpeername(uvPipe, buf, size));
	}

	/**
		Set the number of pending pipe instance handles when the pipe server is
		waiting for connections.
	**/
	public function pendingInstances(count:Int) {
		UV.pipe_pending_instances(uvPipe, count);
	}

	public function pendingCount():Int {
		return UV.pipe_pending_count(uvPipe).resolve();
	}

	/**
		Used to receive handles over IPC pipes.

		First - call `pipe.pendingCount()`, if it’s > 0 then initialize a handle
		of the given type, returned by `pipe.pendingType()` and call `pipe.accept(handle)`.
	**/
	public function pendingType():HandleType {
		return switch UV.pipe_pending_type(uvPipe) {
			case UV_UNKNOWN_HANDLE: UNKNOWN_HANDLE;
			case UV_ASYNC: ASYNC;
			case UV_CHECK: CHECK;
			case UV_FS_EVENT: FS_EVENT;
			case UV_FS_POLL: FS_POLL;
			case UV_HANDLE: HANDLE;
			case UV_IDLE: IDLE;
			case UV_NAMED_PIPE: NAMED_PIPE;
			case UV_POLL: POLL;
			case UV_PREPARE: PREPARE;
			case UV_PROCESS: PROCESS;
			case UV_STREAM: STREAM;
			case UV_TCP: TCP;
			case UV_TIMER: TIMER;
			case UV_TTY: TTY;
			case UV_UDP: UDP;
			case UV_SIGNAL: SIGNAL;
			case UV_FILE: FILE;
			case UV_HANDLE_TYPE_MAX: UNKNOWN_HANDLE;
		}
	}


	/**
		Alters pipe permissions, allowing it to be accessed from processes run by
		different users.
	**/
	public function chmod(mode:PipeMode) {
		UV.pipe_chmod(uvPipe, mode).resolve();
	}
}