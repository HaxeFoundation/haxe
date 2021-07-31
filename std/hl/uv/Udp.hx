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

enum abstract UdpMembership(Int) {
	var LEAVE_GROUP = 0;
	var JOIN_GROUP = 1;
}

/**
	Flags for the callback of `Udbp.recvStart`.

	@see http://docs.libuv.org/en/v1.x/udp.html#c.uv_udp_flags
**/
typedef RecvFlags = {
	mmsgChunk:Bool,
	mmsgFree:Bool,
	partial:Bool,
};

/**
	UDP handles encapsulate UDP communication for both clients and servers.

	@see http://docs.libuv.org/en/v1.x/udp.html
**/
@:forward
abstract Udp(Stream) to Stream to Handle {

	/**
		Initialize a new UDP handle. The actual socket is created lazily.
	**/
	@:hlNative("uv", "udp_init_wrap")
	static public function init(loop:Loop, ?domain:AddressFamily, ?recvmmsg:Bool):Udp
		return null;

	/**
		Bind the handle to an address and port.
	**/
	@:hlNative("uv", "udp_bind_wrap")
	public function bind(addr:SockAddr, ?ipv6Only:Bool, ?reuseAddr:Bool):Void {}

	/**
		Associate the UDP handle to a remote address and port, so every message
		sent by this handle is automatically sent to that destination.

		Calling this function with a `null` addr disconnects the handle.
	**/
	@:hlNative("uv", "udp_connect_wrap")
	public function connect(addr:Null<SockAddr>):Void {}

	/**
		Get the current address to which the handle is bound.
	**/
	@:hlNative("uv", "udp_getsockname_wrap")
	public function getSockName():SockAddr
		return null;

	/**
		Get the address of the peer connected to the handle.
	**/
	@:hlNative("uv", "udp_getpeername_wrap")
	public function getPeerName():SockAddr
		return null;

	/**
		Set membership for a multicast address.
	**/
	@:hlNative("uv", "udp_set_membership_wrap")
	public function setMembership(multicastAddr:String, interfaceAddr:String, membership:UdpMembership):Void {}

	/**
		Set membership for a source-specific multicast group.
	**/
	@:hlNative("uv", "udp_set_source_membership_wrap")
	public function setSourceMembership(multicastAddr:String, interfaceAddr:String, sourceAddr:String, membership:UdpMembership):Void {}

	/**
		Set IP multicast loop flag. Makes multicast packets loop back to local sockets.
	**/
	@:hlNative("uv", "udp_set_multicast_loop_wrap")
	public function setMulticastLoop(on:Bool):Void {}

	/**
		Set the multicast ttl.
	**/
	@:hlNative("uv", "udp_set_multicast_ttl_wrap")
	public function setMulticastTtl(ttl:Int):Void {}

	/**
		Set the multicast interface to send or receive data on.
	**/
	@:hlNative("uv", "udp_set_multicast_interface_wrap")
	public function setMulticastInterface(interfaceAddr:String):Void {}

	/**
		Set broadcast on or off.
	**/
	@:hlNative("uv", "udp_set_broadcast_wrap")
	public function setBroadcast(on:Bool):Void {}

	/**
		Set the time to live.
	**/
	@:hlNative("uv", "udp_set_ttl_wrap")
	public function setTtl(ttl:Int):Void {}

	/**
		Send data over the UDP socket.

		If the socket has not previously been bound with `bind()` it will be bound
		to 0.0.0.0 and a random port number.

		For connected UDP handles, `addr` must be set to `null`.
		For connectionless UDP handles, `addr` cannot be `null`.
	**/
	@:hlNative("uv", "udp_send_wrap")
	public function send(bytes:hl.Bytes, length:Int, addr:Null<SockAddr>, callback:(e:UVError)->Void):Void {}

	/**
		Same as `send()`, but won’t queue a send request if it can’t be completed
		immediately.

		For connected UDP handles, `addr` must be set to `null`.
		For connectionless UDP handles, `addr` cannot be `null`.

		Returns number of bytes sent.
	**/
	@:hlNative("uv", "udp_try_send_wrap")
	public function trySend(bytes:hl.Bytes, length:Int, addr:Null<SockAddr>):Int
		return 0;

	/**
		Prepare for receiving data.

		If the socket has not previously been bound with `bind()` it is bound to
		0.0.0.0 and a random port number.

		If `bytesRead` is 0 it means there is no more data to read. Note that 0
		may also mean that an empty datagram was received (in this case `addr` is
		not `null`).
	**/
	public inline function recvStart(callback:(e:UVError, data:Bytes, bytesRead:Int, addr:Null<SockAddr>, flags:RecvFlags)->Void):Void
		recvStartWrap(callback);

	@:hlNative("uv", "udp_recv_start_wrap")
	public function recvStartWrap(callback:(e:UVError, data:Bytes, bytesRead:Int, addr:Null<SockAddr>, flags:Dynamic)->Void):Void {}

	/**
		Returns `true` if the UDP handle was created with the UV_UDP_RECVMMSG flag
		(`recvmmsg` option in `Udp.init()`) and the platform supports recvmmsg(2).
	**/
	@:hlNative("uv", "udp_using_recvmmsg_wrap")
	public function usingRecvmmsg():Bool
		return false;

	/**
		Stop listening for incoming datagrams.
	**/
	@:hlNative("uv", "udp_recv_stop_wrap")
	public function recvStop():Void {}

	/**
		Number of bytes queued for sending.
		This field strictly shows how much information is currently queued.
	**/
	@:hlNative("uv", "udp_get_send_queue_size_wrap")
	public function getSendQueueSize():Int
		return 0;

	/**
		Number of send requests currently in the queue awaiting to be processed.
	**/
	@:hlNative("uv", "udp_get_send_queue_count_wrap")
	public function getSendQueueCount():Int
		return 0;

}
