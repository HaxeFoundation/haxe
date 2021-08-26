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
import hl.uv.Request;

enum abstract UdpMembership(Int) {
	var LEAVE_GROUP = 0;
	var JOIN_GROUP;
}

enum abstract UdpFlags(Int) to Int {
	/** Disables dual stack mode. */
	var UV_UDP_IPV6ONLY = 1;
	/**
		Indicates message was truncated because read buffer was too small. The
		remainder was discarded by the OS. Used in uv_udp_recv_cb.
	**/
	var UV_UDP_PARTIAL = 2;
	/**
		Indicates if SO_REUSEADDR will be set when binding the handle.
		This sets the SO_REUSEPORT socket flag on the BSDs and OS X. On other
		Unix platforms, it sets the SO_REUSEADDR flag.  What that means is that
		multiple threads or processes can bind to the same address without error
		(provided they all set the flag) but only the last one to bind will receive
		any traffic, in effect "stealing" the port from the previous listener.
	**/
	var UV_UDP_REUSEADDR = 4;
	/**
		Indicates that the message was received by recvmmsg, so the buffer provided
		must not be freed by the recv_cb callback.
	**/
	var UV_UDP_MMSG_CHUNK = 8;
	/**
		Indicates that the buffer provided has been fully utilized by recvmmsg and
		that it should now be freed by the recv_cb callback. When this flag is set
		in uv_udp_recv_cb, nread will always be 0 and addr will always be NULL.
	**/
	var UV_UDP_MMSG_FREE = 16;
	/**
		Indicates if IP_RECVERR/IPV6_RECVERR will be set when binding the handle.
		This sets IP_RECVERR for IPv4 and IPV6_RECVERR for IPv6 UDP sockets on
		Linux. This stops the Linux kernel from suppressing some ICMP error
		messages and enables full ICMP error reporting for faster failover.
		This flag is no-op on platforms other than Linux.
	**/
	var UV_UDP_LINUX_RECVERR = 32;
	/** Indicates that recvmmsg should be used, if available. */
	var UV_UDP_RECVMMSG = 256;
}

@:allow(hl.uv.Udp)
private class UdpSendRequest extends Request<UvUdpSendTStar> {
	var callback:(status:Int)->Void;
	//to keep bytes alive untile send request is complete
	var data:Bytes;
}

/**
	Flags for the callback of `Udbp.recvStart`.

	@see http://docs.libuv.org/en/v1.x/udp.html#c.uv_udp_flags
**/
abstract RecvFlags(Int) to Int {
	@:allow(hl.uv.Udp)
	inline function new(flags:Int) {
		this = flags;
	}

	/**
		Indicates that the buffer provided has been fully utilized by recvmmsg.
		When this flag is set in `Udp.recvStart` callback, `bytesRead` will always
		be 0 and `addr` will always be null.
	**/
	public var mmsgChunk(get,never):Bool;
	inline function get_mmsgChunk() return 0 != this & UV_UDP_MMSG_CHUNK;

	/**
		Indicates that the buffer provided has been fully utilized by recvmmsg.
		When this flag is set in `Udp.recvStart` callback, `bytesRead` will always
		be 0 and `addr` will always be null.
	**/
	public var mmsgFree(get,never):Bool;
	inline function get_mmsgFree() return 0 != this & UV_UDP_MMSG_FREE;

	/**
		Indicates message was truncated because read buffer was too small. The
		remainder was discarded by the OS.
	**/
	public var partial(get,never):Bool;
	inline function get_partial() return 0 != this & UV_UDP_PARTIAL;
}

/**
	UDP handles encapsulate UDP communication for both clients and servers.

	@see http://docs.libuv.org/en/v1.x/udp.html
**/
class Udp extends Handle<UvUdpTStar> {
	var onAlloc:(buf:Buffer, size:Int)->Void;
	var onRecv:(nRead:I64, buf:UvBufTArr, addr:Null<CSockaddrStar>, flags:UInt)->Void;

	function new(handle:UvUdpTStar) {
		super(handle);
		onAlloc = (buf, size) -> buf.set(new Bytes(size), size);
	}

	/**
		Initialize a new UDP handle. The actual socket is created lazily.
	**/
	static public function init(loop:Loop, ?domain:AddressFamily, ?recvmmsg:Bool):Udp {
		loop.checkLoop();
		var udp = new Udp(UV.alloc_udp());
		var result = if(domain == null && recvmmsg == null) {
			loop.udp_init(udp.h);
		} else {
			var flags = 0;
			if(domain != null)
				flags |= domain.address_family_to_af();
			if(recvmmsg)
				flags |= UV_UDP_RECVMMSG;
			loop.udp_init_ex(udp.h, flags);
		}
		if(result < 0) {
			udp.freeHandle();
			result.throwErr();
		}
		return udp;
	}

	/**
		Bind the handle to an address and port.
	**/
	public function bind(addr:SockAddr, ipv6Only:Bool = false, reuseAddr:Bool = false):Void {
		handle(h -> {
			var flags = 0;
			if(ipv6Only)
				flags |= UV_UDP_IPV6ONLY;
			if(reuseAddr)
				flags |= UV_UDP_REUSEADDR;
			h.udp_bind(addr, flags).resolve();
		});
	}

	/**
		Associate the UDP handle to a remote address and port, so every message
		sent by this handle is automatically sent to that destination.

		Calling this function with a `null` addr disconnects the handle.
	**/
	public function connect(addr:Null<SockAddr>):Void {
		handle(h -> {
			h.udp_connect(addr).resolve();
		});
	}

	/**
		Get the current address to which the handle is bound.
	**/
	public function getSockName():SockAddr {
		return handleReturn(h -> {
			var addr = UV.alloc_sockaddr_storage();
			var size = UV.sockaddr_storage_size();
			var result = h.udp_getsockname(addr.sockaddr_of_storage(), Ref.make(size));
			if(result < 0) {
				addr.sockaddr_storage_to_pointer().free();
				result.throwErr();
			}
			return addr;
		});
	}

	/**
		Get the address of the peer connected to the handle.
	**/
	public function getPeerName():SockAddr {
		return handleReturn(h -> {
			var addr = UV.alloc_sockaddr_storage();
			var size = UV.sockaddr_storage_size();
			var result = h.udp_getpeername(addr.sockaddr_of_storage(), Ref.make(size));
			if(result < 0) {
				addr.sockaddr_storage_to_pointer().free();
				result.throwErr();
			}
			return addr;
		});
	}

	/**
		Set membership for a multicast address.
	**/
	public function setMembership(multicastAddr:String, interfaceAddr:String, membership:UdpMembership):Void {
		handle(h -> {
			h.udp_set_membership(multicastAddr.toUTF8(), interfaceAddr.toUTF8(), membership).resolve();
		});
	}

	/**
		Set membership for a source-specific multicast group.
	**/
	public function setSourceMembership(multicastAddr:String, interfaceAddr:String, sourceAddr:String, membership:UdpMembership):Void {
		handle(h -> {
			h.udp_set_source_membership(multicastAddr.toUTF8(), interfaceAddr.toUTF8(), sourceAddr.toUTF8(), membership).resolve();
		});
	}

	/**
		Set IP multicast loop flag. Makes multicast packets loop back to local sockets.
	**/
	public function setMulticastLoop(on:Bool):Void {
		handle(h -> h.udp_set_multicast_loop(on ? 1 : 0).resolve());
	}

	/**
		Set the multicast ttl.
	**/
	public function setMulticastTtl(ttl:Int):Void {
		handle(h -> h.udp_set_multicast_ttl(ttl).resolve());
	}

	/**
		Set the multicast interface to send or receive data on.
	**/
	public function setMulticastInterface(interfaceAddr:String):Void {
		handle(h -> h.udp_set_multicast_interface(interfaceAddr.toUTF8()).resolve());
	}

	/**
		Set broadcast on or off.
	**/
	public function setBroadcast(on:Bool):Void {
		handle(h -> h.udp_set_broadcast(on ? 1 : 0).resolve());
	}

	/**
		Set the time to live.
	**/
	public function setTtl(ttl:Int):Void {
		handle(h -> h.udp_set_ttl(ttl).resolve());
	}

	/**
		Send data over the UDP socket.

		If the socket has not previously been bound with `bind()` it will be bound
		to 0.0.0.0 and a random port number.

		For connected UDP handles, `addr` must be set to `null`.
		For connectionless UDP handles, `addr` cannot be `null`.
	**/
	public function send(bytes:hl.Bytes, length:Int, addr:Null<SockAddr>, callback:(e:UVError)->Void):Void {
		handle(h -> {
			var req = new UdpSendRequest(UV.alloc_udp_send());
			var buf = UV.alloc_buf(bytes, length);
			var result = req.r.udp_send_with_cb(h, buf, 1, addr);
			if(result < 0) {
				buf.buf_to_pointer().free();
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
		Same as `send()`, but won’t queue a send request if it can’t be completed
		immediately.

		For connected UDP handles, `addr` must be set to `null`.
		For connectionless UDP handles, `addr` cannot be `null`.

		Returns number of bytes sent.
	**/
	public function trySend(bytes:hl.Bytes, length:Int, addr:Null<SockAddr>):Int {
		return handleReturn(h -> {
			var buf = UV.alloc_buf(bytes, length);
			var result = h.udp_try_send(buf, 1, addr);
			buf.buf_to_pointer().free();
			return result.resolve();
		});
	}

	/**
		Set buffer allocation function for `recvStart`.

		A suggested size (65536 at the moment in most cases) is provided, but it’s
		just an indication, not related in any way to the pending data to be read.
		The user is free to allocate the amount of memory they decide.

		By default it's `(buf, size -> buf.set(new hl.Bytes(size), size)`
	**/
	public function setAlloc(alloc:(buf:Buffer, size:Int)->Void) {
		onAlloc = alloc;
	}

	/**
		Prepare for receiving data.

		If the socket has not previously been bound with `bind()` it is bound to
		0.0.0.0 and a random port number.

		If `bytesRead` is 0 it means there is no more data to read. Note that 0
		may also mean that an empty datagram was received (in this case `addr` is
		not `null`).
	**/
	public function recvStart(callback:(e:UVError, data:Null<Bytes>, bytesRead:Int, addr:Null<SockAddr>, flags:RecvFlags)->Void):Void {
		handle(h -> {
			h.udp_recv_start_with_cb().resolve();
			onRecv = (nRead, buf, addr, flags) -> { // TODO: free addr
				var bytesRead = nRead.toInt();
				var e = bytesRead.translate_uv_error();
				var data = switch e {
					case UV_NOERR:
						buf.buf_base();
					case _:
						bytesRead = 0;
						null;
				}
				callback(e, data, bytesRead, addr, new RecvFlags(flags));
			}
		});
	}

	/**
		Returns `true` if the UDP handle was created with the UV_UDP_RECVMMSG flag
		(`recvmmsg` option in `Udp.init()`) and the platform supports recvmmsg(2).
	**/
	public function usingRecvmmsg():Bool {
		return handleReturn(h -> h.udp_using_recvmmsg() == 1);
	}

	/**
		Stop listening for incoming datagrams.
	**/
	public function recvStop():Void {
		handle(h -> h.udp_recv_stop().resolve());
	}

	/**
		Number of bytes queued for sending.
		This field strictly shows how much information is currently queued.
	**/
	public function getSendQueueSize():Int {
		return handleReturn(h -> h.udp_get_send_queue_size().toInt());
	}

	/**
		Number of send requests currently in the queue awaiting to be processed.
	**/
	public function getSendQueueCount():Int {
		return handleReturn(h -> h.udp_get_send_queue_count().toInt());
	}
}
