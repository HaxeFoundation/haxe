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

enum abstract TcpFlag(Int) to Int {
	/** Used with uv_tcp_bind, when an IPv6 address is used. */
	var UV_TCP_IPV6ONLY = 1;
}

/**
	TCP handles are used to represent both TCP streams and servers.

	@see http://docs.libuv.org/en/v1.x/tcp.html
**/
class Tcp extends Stream<UvTcpTStar> {

	/**
		Initialize the handle. No socket is created as of yet.

		Omitting `domain` is the same as passing `AddressFamily.UNSPEC` to it,
		which means no socket is created yet.
	**/
	static public function init(loop:Loop, ?domain:AddressFamily):Tcp {
		loop.checkLoop();
		var tcp = new Tcp(UV.alloc_tcp());
		var result = domain == null ? loop.tcp_init(tcp.h) : loop.tcp_init_ex(tcp.h, domain.address_family_to_af());
		if(result < 0) {
			tcp.freeHandle();
			result.throwErr();
		}
		return tcp;
	}

	/**
		This call is used in conjunction with `listen()` to accept incoming connections.
		Call this function after receiving a listen callback to accept the connection.

		Server(this stream) and `client` must be handles running on the same loop.

		@see http://docs.libuv.org/en/v1.x/stream.html#c.uv_accept
	**/
	public function accept(client:Tcp):Void {
		handle(server -> client.handle(client -> {
			server.accept(client).resolve();
		}));
	}

	/**
		Enable TCP_NODELAY.
	**/
	public function noDelay(enable:Bool):Void {
		handle(h -> h.tcp_nodelay(enable ? 1 : 0).resolve());
	}

	/**
		Enable / disable TCP keep-alive.
		`delay` is the initial delay in seconds, ignored when `enable` is `false`.
	**/
	public function keepAlive(enable:Bool, delay:Int):Void {
		handle(h -> h.tcp_keepalive((enable ? 1 : 0), delay).resolve());
	}

	/**
		Enable / disable simultaneous asynchronous accept requests that are queued
		by the operating system when listening for new TCP connections.
	**/
	public function simultaneousAccepts(enable:Bool):Void {
		handle(h -> h.tcp_simultaneous_accepts(enable ? 1 : 0).resolve());
	}

	/**
		Bind the handle to an address and port.
	**/
	public function bind(addr:SockAddr, ?ipv6Only:Bool):Void {
		handle(h -> h.tcp_bind(addr, (ipv6Only ? UV_TCP_IPV6ONLY : 0)).resolve());
	}

	/**
		Get the current address to which the handle is bound.
	**/
	public function getSockName():SockAddr {
		return handleReturn(h -> {
			var addr = UV.alloc_sockaddr_storage(); // TODO: need to free addr manually?
			var size = UV.sockaddr_storage_size();
			var result = h.tcp_getsockname(addr.sockaddr_of_storage(), Ref.make(size));
			if(result < 0) {
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
			var addr = UV.alloc_sockaddr_storage(); // TODO: need to free addr manually?
			var size = UV.sockaddr_storage_size();
			var result = h.tcp_getpeername(addr.sockaddr_of_storage(), Ref.make(size));
			if(result < 0) {
				result.throwErr();
			}
			return addr;
		});
	}

	/**
		Establish an IPv4 or IPv6 TCP connection.
	**/
	public function connect(addr:SockAddr, callback:(e:UVError)->Void):Void {
		handle(h -> {
			var req = Stream.createConnect();
			var result = req.r.tcp_connect_with_cb(h, addr);
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
		Resets a TCP connection by sending a RST packet.

		@see http://docs.libuv.org/en/v1.x/tcp.html#c.uv_tcp_close_reset
	**/
	public function closeReset(?callback:()->Void):Void {
		handle(h -> {
			if(h.is_closing() != 0)
				throw new UVException(UV_EINVAL);
			onClose = () -> {
				freeHandle();
				if(callback != null)
					callback();
			}
			h.tcp_close_reset_with_cb().resolve();
		});
	}
}
