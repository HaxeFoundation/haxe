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
class WriteRequest extends Request {
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
class ConnectRequest extends Request {
	var uvConnect:RawPointer<UvConnectT>;
	var onConnect:(e:UVError)->Void;

	function setupUvReq() {
		uvConnect = UvConnectT.create();
		uvReq = cast uvConnect;
	}
}

@:allow(cpp.uv)
class ShutdownRequest extends Request {
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
	@:allow(cpp.uv) var uvStream:RawPointer<UvStreamT>;
	var onConnection:(e:UVError)->Void;
	var onRead:(e:UVError, data:Bytes, bytesRead:SSizeT)->Void;
	var onAlloc:(suggestedSize:SizeT)->Bytes;
	//to keep bytes alive while waiting for a callback
	var readBuffer:Bytes;

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
		var req:ShutdownRequest = cast Request.getRequest(cast uvShutdown);
		req.onShutdown(status.explain());
	}

	/**
		Start listening for incoming connections.

		`backlog` indicates the number of connections the kernel might queue.
		When a new incoming connection is received the `callback` is called.
	**/
	public function listen(backlog:Int, callback:(e:UVError)->Void) {
		UV.listen(uvStream, backlog, Callable.fromStaticFunction(uvConnectionCb)).resolve();
		onConnection = callback;
	}

	static function uvConnectionCb(uvStream:RawPointer<UvStreamT>, status:Int) {
		var stream:Stream = cast Handle.getHandle(cast uvStream);
		stream.onConnection(status.explain());
	}

	/**
		Read data from an incoming stream.
	**/
	public function readStart(callback:(e:UVError, data:Bytes, bytesRead:SSizeT)->Void, ?allocate:(size:SizeT)->Bytes) {
		UV.read_start(uvStream, Callable.fromStaticFunction(uvAllocCb), Callable.fromStaticFunction(uvReadCb)).resolve();
		onRead = callback;
		onAlloc = size -> allocate == null ? Bytes.alloc(size) : allocate(size);
	}

	static function uvReadCb(uvStream:RawPointer<UvStreamT>, bytesRead:SSizeT, buf:RawConstPointer<UvBufT>) {
		var stream:Stream = cast Handle.getHandle(cast uvStream);
		var data = stream.readBuffer;
		stream.readBuffer = null;
		stream.onRead(bytesRead.explain(), data, bytesRead < 0 ? 0 : bytesRead);
	}

	static function uvAllocCb(uvHandle:RawPointer<UvHandleT>, size:SizeT, buf:RawPointer<UvBufT>) {
		var stream:Stream = cast Handle.getHandle(cast uvHandle);
		stream.readBuffer = stream.onAlloc(size);
		var ref = Pointer.fromRaw(buf).ref;
		ref.base = NativeArray.getBase(stream.readBuffer.getData()).getBase();
		ref.len = stream.readBuffer.length;
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
		Write data to stream.
	**/
	public function write(data:Bytes, pos:UInt, length:UInt, callback:(e:UVError)->Void) {
		writeImpl(data, pos, length, callback, (r, h, b, cb) -> UV.write(r, h, b, 1, cb));
	}

	inline function writeImpl(data:Bytes, pos:UInt, length:UInt, callback:(e:UVError)->Void, fn:(r:RawPointer<UvWriteT>, h:RawPointer<UvStreamT>, b:RawPointer<UvBufT>, cb:UvWriteCb)->Int) {
		if(pos + length > data.length)
			throw new UVException(UV_ENOBUFS);
		var req = new WriteRequest();
		req.buf = UvBufT.create();
		var ptr = Pointer.fromRaw(req.buf);
		var base = NativeArray.getBase(data.getData()).getBase();
		ptr.value.base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		ptr.value.len = length;
		fn(req.uvWrite, uvStream, req.buf, Callable.fromStaticFunction(uvWriteCb)).resolve();
		req.data = data;
		req.onWrite = callback;
	}

	static function uvWriteCb(uvWrite:RawPointer<UvWriteT>, status:Int) {
		var req:WriteRequest = cast Request.getRequest(cast uvWrite);
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
		base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		var buf = RawPointer.addressOf(UV.buf_init(base, length));
		var result = UV.try_write(uvStream, buf, 1);
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
	public function isWritable():Bool {
		return 0 != UV.is_writable(uvStream);
	}
}