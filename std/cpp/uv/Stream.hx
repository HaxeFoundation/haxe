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

using cpp.uv.UV;

@:allow(cpp.uv)
private class WriteRequest extends Request {
	var uvWrite:RawPointer<UvWriteT>;
	var onWrite:(e:UVError)->Void;
	//to keep bytes alive while waiting for a callback
	var data:Bytes;
	var buf:RawPointer<UvBufT>;

	function setupUvReq() {
		uvWrite = UvWriteT.create();
		uvReq = cast uvWrite;
	}

	override function destructor() {
		super.destructor();
		if(buf != null)
			Stdlib.free(Pointer.fromRaw(buf));
	}
}

@:allow(cpp.uv)
private class ConnectRequest extends Request {
	var uvConnect:RawPointer<UvConnectT>;
	var onConnect:(e:UVError)->Void;

	function setupUvReq() {
		uvConnect = UvConnectT.create();
		uvReq = cast uvConnect;
	}
}

@:allow(cpp.uv)
private class ShutdownRequest extends Request {
	var uvShutdown:RawPointer<UvShutdownT>;
	var onShutdown:(e:UVError)->Void;

	function setupUvReq() {
		uvShutdown = UvShutdownT.create();
		uvReq = cast uvShutdown;
	}
}

/**
	Stream handles provide an abstraction of a duplex communication channel.

	@see http://docs.libuv.org/en/v1.x/stream.html
**/
@:headerCode('#include "uv.h"')
abstract class Stream extends Handle {
	var uvStream:RawPointer<UvStreamT>;
	var onConnection:(e:UVError)->Void;
	var onRead:(e:UVError, data:Bytes, bytesRead:SSizeT)->Void;
	var onAlloc:(suggestedSize:SizeT)->Bytes;
	//to keep bytes alive while waiting for a callback
	var readBuffer:Bytes;

	@:allow(cpp.uv) static function createWriteRequest():WriteRequest {
		return new WriteRequest();
	}

	@:allow(cpp.uv) static function createConnectRequest():ConnectRequest {
		return new ConnectRequest();
	}

	/**
		Shutdown the outgoing (write) side of a duplex stream.
		It waits for pending write requests to complete.
	**/
	public function shutdown(callback:(e:UVError)->Void) {
		var req = new ShutdownRequest();
		UV.shutdown(req.uvShutdown, uvStream, Callable.fromStaticFunction(uvShutdownCb)).resolve();
		req.onShutdown = callback;
	}

	static function uvShutdownCb(uvShutdown:RawPointer<UvShutdownT>, status:Int) {
		var req = Std.downcast(Request.getRequest(cast uvShutdown), ShutdownRequest);
		req.onShutdown(status.explain());
	}

	/**
		Start listening for incoming connections.

		`backlog` indicates the number of connections the kernel might queue.
		When a new incoming connection is received the `callback` is called.
	**/
	public function listen(backlog:Int, callback:(e:UVError)->Void) {
		UV.shutdown(uvStream, Callable.fromStaticFunction(uvConnectionCb)).resolve();
		onConnection = callback;
	}

	static function uvConnectionCb(uvStream:RawPointer<UvStreamT>, status:Int) {
		var stream = Std.downcast(Request.getHandle(cast uvStream), Stream);
		stream.onConnection(status.explain());
	}

	/**
		Read data from an incoming stream.
	**/
	public function readStart(callback:(e:UVError, data:Bytes, bytesRead:SSizeT)->Void, ?allocate:(size:SizeT)->Bytes) {
		UV.read_start(uvStream, Callable.fromStaticFunction(uvReadCb), Callable.fromStaticFunction(uvAllocCb)).resolve();
		onRead = callback;
		onAlloc = size -> readBuffer = allocate == null ? Bytes.alloc(size) : allocate(size);
	}

	static function uvReadCb(uvStream:RawPointer<UvStreamT>, bytesRead:SSizeT, buf:RawPointer<UvBufT>) {
		var stream = Std.downcast(Request.getRequest(cast uvStream), Stream);
		Stdlib.free(Pointer.fromRaw(buf));
		var data = readBuffer;
		readBuffer = null;
		stream.onRead(bytesRead.explain(), data, bytesRead < 0 ? 0 : bytesRead);
	}

	static function uvAllocCb(uvStream:RawPointer<UvStreamT>, size:SizeT, buf:RawPointer<UvBufT>) {
		var stream = Std.downcast(Request.getRequest(cast uvStream), Stream);
		readBuffer = stream.onAlloc(size);
		var base = NativeArray.getBase(readBuffer.getData()).getBase();
		return RawPointer.addressOf(UV.buf_init(base, (size:Int)));
	}

	/**
		Stop reading data from the stream.
	**/
	public function readStop() {
		UV.read_stop(uvStream).resolve();
		onRead = null;
		onAlloc = null;
		readBuffer = null;
	}

	/**
		Shutdown the outgoing (write) side of a duplex stream.
		It waits for pending write requests to complete.
	**/
	public function write(data:Bytes, pos:UInt, length:UInt, callback:(e:UVError)->Void) {
		if(pos + length > data.length)
			throw new UVException(UV_ENOBUFS);
		var req = new WriteRequest();
		var base = NativeArray.getBase(readBuffer.getData()).getBase();
		req.buf = RawPointer.addressOf(UV.buf_init(Pointer.fromRaw(base).at(pos), length));
		UV.write(req.uvWrite, uvStream, req.buf, 1, Callable.fromStaticFunction(uvWriteCb)).resolve();
		req.data = data;
		req.onWrite = callback;
	}

	static function uvWriteCb(uvWrite:RawPointer<UvWriteT>, status:Int) {
		var req = Std.downcast(Request.getRequest(cast uvWrite), WriteRequest);
		req.onWrite(status.explain());
	}

	/**
		Same as `Stream.write()`, but won’t queue a write request if it can’t be
		completed immediately.

		Returns number of bytes written.
	**/
	public function tryWrite(data:Bytes, pos:UInt, length:UInt):Int {
		if(pos + length > data.length)
			throw new UVException(UV_ENOBUFS);
		var base = NativeArray.getBase(readBuffer.getData()).getBase();
		var buf = RawPointer.addressOf(UV.buf_init(Pointer.fromRaw(base).at(pos), length));
		var result = UV.tryWrite(uvStream, buf, 1);
		Stdlib.free(Pointer.fromRaw(buf));
		return result.resolve();
	}

	/**
		Returns `true` if the stream is readable
	**/
	public function isReadable():Bool {
		return 0 != UV.is_readable(uvStream);
	}

	/**
		Returns `true` if the stream is writable
	**/
	public function isReadable():Bool {
		return 0 != UV.is_writable(uvStream);
	}
}