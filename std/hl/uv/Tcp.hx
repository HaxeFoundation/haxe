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

/**
	TCP handles are used to represent both TCP streams and servers.

	@see http://docs.libuv.org/en/v1.x/tcp.html
**/
@:forward
abstract Tcp(Stream) to Stream to Handle {

	/**
		Initialize the handle. No socket is created as of yet.

		Omitting `domain` is the same as passing `AddressFamily.UNSPEC` to it,
		which means no socket is created yet.
	**/
	@:hlNative("uv", "tcp_init_wrap")
	static public function init(loop:Loop, ?domain:AddressFamily):Tcp
		return null;

	/**
		This call is used in conjunction with `listen()` to accept incoming connections.
		Call this function after receiving a listen callback to accept the connection.

		Server(this stream) and `client` must be handles running on the same loop.

		@see http://docs.libuv.org/en/v1.x/stream.html#c.uv_accept
	**/
	@:hlNative("uv", "accept_wrap")
	public function accept(client:Tcp):Void {}

	/**
		Enable TCP_NODELAY.
	**/
	@:hlNative("uv", "tcp_noDelay_wrap")
	public function noDelay(enable:Bool):Void {}

	/**
		Enable / disable TCP keep-alive.
		`delay` is the initial delay in seconds, ignored when `enable` is `false`.
	**/
	@:hlNative("uv", "tcp_keepalive_wrap")
	public function keepAlive(enable:Bool, delay:Int):Void {}

	/**
		Enable / disable simultaneous asynchronous accept requests that are queued
		by the operating system when listening for new TCP connections.
	**/
	@:hlNative("uv", "tcp_simultaneous_accepts_wrap")
	public function simultaneousAccepts(enable:Bool):Void {}

	/**
		Bind the handle to an address and port.
	**/
	@:hlNative("uv", "tcp_bind_wrap")
	public function bind(addr:SockAddr, ?ipv6Only:Bool):Void {}

	/**
		Get the current address to which the handle is bound.
	**/
	@:hlNative("uv", "tcp_getsockname_wrap")
	public function getSockName():SockAddr
		return null;

	/**
		Get the address of the peer connected to the handle.
	**/
	@:hlNative("uv", "tcp_getpeername_wrap")
	public function getPeerName():SockAddr
		return null;

	/**
		Establish an IPv4 or IPv6 TCP connection.
	**/
	@:hlNative("uv", "tcp_connect_wrap")
	public function connect(addr:SockAddr, callback:(e:UVError)->Void):Void {}

	/**
		Resets a TCP connection by sending a RST packet.

		@see http://docs.libuv.org/en/v1.x/tcp.html#c.uv_tcp_close_reset
	**/
	@:hlNative("uv", "tcp_close_reset_wrap")
	public function closeReset(?callback:()->Void):Void {}
}
