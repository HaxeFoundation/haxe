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
import cpp.uv.SockAddr;
import cpp.uv.Udp;

using cpp.uv.UV;

enum abstract UdpBindFlag(Int) {
	var IPV6ONLY;
	var REUSEADDR;
	var RECVMMSG;
}

enum abstract UdpMembership(Int) {
	var LEAVE;
	var JOIN;
}

/**
	Flags for the callback of `Udbp.recvStart`.
	@see http://docs.libuv.org/en/v1.x/udp.html#c.uv_udp_flags
**/
@:headerCode('#include "uv.h"')
abstract RecvFlags(Int) to Int {
	@:allow(cpp.uv.Udp)
	inline function new(flags:Int) {
		this = flags;
	}

	/**
		Indicates that the buffer provided has been fully utilized by recvmmsg.
		When this flag is set in `Udp.recvStart` callback, `bytesReceived` will always
		be 0 and `addr` will always be null.
	**/
	public var mmsgChunk(get,never):Bool;
	function get_mmsgChunk() return 0 != this & UV_UDP_MMSG_CHUNK;

	/**
		Indicates that the buffer provided has been fully utilized by recvmmsg.
		When this flag is set in `Udp.recvStart` callback, `bytesReceived` will always
		be 0 and `addr` will always be null.
	**/
	public var mmsgFree(get,never):Bool;
	function get_mmsgFree() return 0 != this & UV_UDP_MMSG_FREE;

	/**
		Indicates message was truncated because read buffer was too small. The
		remainder was discarded by the OS.
	**/
	public var partial(get,never):Bool;
	function get_partial() return 0 != this & UV_UDP_PARTIAL;
}

@:allow(cpp.uv)
private class SendRequest extends Request {
	var uvSend:RawPointer<UvUdpSendT>;
	var onSend:(e:UVError)->Void;
	//to keep bytes alive while waiting for a callback
	var data:Bytes;
	var buf:RawPointer<UvBufT>;

	function setupUvReq() {
		uvSend = UvUdpSendT.create();
		uvReq = cast uvSend;
	}

	override function destructor() {
		super.destructor();
		if(buf != null)
			Pointer.fromRaw(buf).destroy();
	}
}

/**
	UDP handles encapsulate UDP communication for both clients and servers.

	@see http://docs.libuv.org/en/v1.x/udp.html
**/
@:headerCode('#include "uv.h"')
class Udp extends Handle {
	var uvUdp:RawPointer<UvUdpT>;
	var onRecv:(e:UVError, data:Bytes, bytesReceived:SSizeT, addr:Null<SockAddr>, flags:RecvFlags)->Void;
	var onAlloc:(suggestedSize:SizeT)->Bytes;
	//to keep bytes alive while waiting for a callback
	var recvBuffer:Bytes;

	function setupUvHandle() {
		uvUdp = UvUdpT.create();
		uvHandle = cast uvUdp;
	}

	/**
		Create a UDP handle.
	**/
	static public function init(loop:Loop, domain:AddressFamily = UNSPEC, recvmmsg:Bool = false):Udp {
		var udp = new Udp();
		var flags:Int = switch domain {
			case UNSPEC: AF_UNSPEC;
			case INET: AF_INET;
			case INET6: AF_INET6;
			case OTHER(i): i;
		}
		if(recvmmsg)
			flags |= UV_UDP_RECVMMSG;
		UV.udp_init_ex(loop.uvLoop, udp.uvUdp, flags).resolve();
		return udp;
	}

	/**
		Bind the UDP handle to an IP address and port.
	**/
	public function bind(addr:SockAddr, ?flags:Array<UdpBindFlag>) {
		var iFlags = 0;
		if(flags != null)
			for(flag in flags)
				iFlags |= switch flag {
					case IPV6ONLY: UV_UDP_IPV6ONLY;
					case REUSEADDR: UV_UDP_REUSEADDR;
					case RECVMMSG: UV_UDP_RECVMMSG;
				}

		UV.udp_bind(uvUdp, cast addr.storage, iFlags).resolve();
	}

	/**
		Associate the UDP handle to a remote address and port, so every message
		sent by this handle is automatically sent to that destination.

		Calling this function with a NULL addr disconnects the handle.
	**/
	public function connect(addr:SockAddr) {
		UV.udp_connect(uvUdp, cast addr.storage).resolve();
	}

	/**
		Get the remote IP and port of the UDP handle on connected UDP handles.
	**/
	public function getPeerName() {
		var addr = new SockAddr();
		var size:Int = untyped __cpp__('sizeof(sockaddr_storage *)');
		UV.udp_getpeername(uvUdp, cast addr.storage, RawPointer.addressOf(size)).resolve();
		return addr;
	}

	/**
		Get the local IP and port of the UDP handle.
	**/
	public function getSockName() {
		var addr = new SockAddr();
		var size:Int = untyped __cpp__('sizeof(sockaddr_storage *)');
		UV.udp_getsockname(uvUdp, cast addr.storage, RawPointer.addressOf(size)).resolve();
		return addr;
	}

	/**
		Resets a UDP connection by sending a RST packet.
	**/
	public function setMembership(multicastAddr:String, interfaceAddr:String, membership:UdpMembership) {
		var membership = switch membership {
			case LEAVE: UV_LEAVE_GROUP;
			case JOIN: UV_JOIN_GROUP;
		}
		UV.udp_set_membership(uvUdp, multicastAddr, interfaceAddr, membership).resolve();
	}

	/**
		Set IP multicast loop flag.
		Makes multicast packets loop back to local sockets.
	**/
	public function setMulticastLoop(on:Bool) {
		UV.udp_set_multicast_loop(uvUdp, on ? 1 : 0).resolve();
	}

	/**
		Set the multicast ttl.
	**/
	public function setMulticastTtl(ttl:Int) {
		UV.udp_set_multicast_ttl(uvUdp, ttl).resolve();
	}

	/**
		Set the multicast interface to send or receive data on.
	**/
	public function setMulticastInterface(interfaceAddr:String) {
		UV.udp_set_multicast_interface(uvUdp, interfaceAddr).resolve();
	}

	/**
		Set broadcast on or off.
	**/
	public function setBroadcast(on:Bool) {
		UV.udp_set_broadcast(uvUdp, on ? 1 : 0).resolve();
	}

	/**
		Set the time to live.
	**/
	public function setTtl(ttl:Int) {
		UV.udp_set_ttl(uvUdp, ttl).resolve();
	}

	/**
		Send data over the UDP socket.
	**/
	public function send(data:Bytes, pos:UInt, length:UInt, addr:Null<SockAddr>, callback:(e:UVError)->Void) {
		if(pos + length > data.length)
			throw new UVException(UV_ENOBUFS);
		var req = new SendRequest();
		req.buf = UvBufT.create();
		var ptr = Pointer.fromRaw(req.buf);
		var base = NativeArray.getBase(data.getData()).getBase();
		ptr.value.base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		ptr.value.len = length;
		UV.udp_send(req.uvSend, uvUdp, req.buf, 1, addr == null ? null : cast addr.storage, Callable.fromStaticFunction(uvSendCb)).resolve();
		req.data = data;
		req.onSend = callback;
	}

	static function uvSendCb(uvSend:RawPointer<UvUdpSendT>, status:Int) {
		var req:SendRequest = cast Request.getRequest(cast uvSend);
		Pointer.fromRaw(req.buf).destroy();
		req.onSend(status.explain());
	}

	/**
		Same as `udp.send()`, but won’t queue a send request if it can’t be completed
		immediately.

		Returns number of bytes sent.
	**/
	public function trySend(data:Bytes, pos:UInt, length:UInt, addr:Null<SockAddr>):Int {
		if(pos + length > data.length)
			throw new UVException(UV_ENOBUFS);
		var base = NativeArray.getBase(data.getData()).getBase();
		base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		var buf = UV.buf_init(base, length);
		return UV.udp_try_send(uvUdp, RawPointer.addressOf(buf), 1, addr == null ? null : cast addr.storage).resolve();
	}

	/**
		Prepare for receiving data.
	**/
	public function recvStart(callback:(e:UVError, data:Bytes, bytesReceived:SSizeT, addr:Null<SockAddr>, flags:RecvFlags)->Void, ?allocate:(size:SizeT)->Bytes) {
		UV.udp_recv_start(uvUdp, Callable.fromStaticFunction(uvAllocCb), Callable.fromStaticFunction(uvUdpRecvCb)).resolve();
		onRecv = callback;
		onAlloc = size -> allocate == null ? Bytes.alloc(size) : allocate(size);
	}

	static function uvUdpRecvCb(uvUdp:RawPointer<UvUdpT>, nread:SSizeT, buf:RawConstPointer<UvBufT>, sockaddr:RawConstPointer<Sockaddr>, flags:UInt32) {
		var udp:Udp = cast Handle.getHandle(cast uvUdp);
		var data = udp.recvBuffer;
		var addr = null;
		if(sockaddr != null) {
			addr = new SockAddr();
			untyped __cpp__('memcpy({0}, {1}, sizeof(struct sockaddr_storage))', addr.storage, sockaddr);
		}
		if((nread <= 0 && sockaddr == null) || (flags & UV_UDP_MMSG_FREE != 0)) {
			udp.recvBuffer = null;
		}
		udp.onRecv(nread.explain(), data, nread < 0 ? 0 : nread, addr, new RecvFlags(flags));
	}

	static function uvAllocCb(uvHandle:RawPointer<UvHandleT>, size:SizeT, buf:RawPointer<UvBufT>) {
		var udp:Udp = cast Handle.getHandle(cast uvHandle);
		udp.recvBuffer = udp.onAlloc(size);
		var ref = Pointer.fromRaw(buf).ref;
		ref.base = NativeArray.getBase(udp.recvBuffer.getData()).getBase();
		ref.len = udp.recvBuffer.length;
	}

	/**
		Returns `true` if the UDP handle was created with the `recvmmsg == true`
		and the platform supports recvmmsg(2)
	**/
	public function usingRecvmmsg():Bool {
		return 0 != UV.udp_using_recvmmsg(uvUdp);
	}

	/**
		Stop listening for incoming datagrams.
	**/
	public function recvStop() {
		UV.udp_recv_stop(uvUdp).resolve();
	}

	/**
		Number of bytes queued for sending.
	**/
	public function getSendQueueSize():Int64 {
		return UV.udp_get_send_queue_size(uvUdp);
	}

	/**
		Number of send requests currently in the queue awaiting to be processed.
	**/
	public function getSendQueueCount():Int64 {
		return UV.udp_get_send_queue_count(uvUdp);
	}
}