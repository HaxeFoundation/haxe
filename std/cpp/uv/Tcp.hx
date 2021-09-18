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

import cpp.uv.SockAddr;

using cpp.uv.UV;

/**
	TCP handles are used to represent both TCP streams and servers.

	@see http://docs.libuv.org/en/v1.x/tcp.html
**/
@:headerCode('#include "uv.h"')
abstract class Tcp extends Tcp {
	var uvTcp:RawPointer<UvTcpT>;

	function setupUvHandle() {
		uvTcp = UvTcpT.create();
		uvStream = cast uvTcp;
		uvHandle = cast uvHandle;
	}

	/**
		Create a TCP handle
	**/
	public function init(loop:Loop, domain:AddressFamily = UNSPEC):Tcp {
		var tcp = new Tcp();
		var flags = switch domain {
			case UNSPEC: AF_UNSPEC;
			case INET: AF_INET;
			case INET6: AF_INET6;
			case OTHER(i): i;
		}
		UV.tcp_init(loop.uvLoop, tcp.uvTcp, flags).resolve();
		return tcp;
	}

	/**
		Enable TCP_NODELAY, which disables Nagle’s algorithm.
	**/
	public function noDelay(enable:Bool) {
		UV.tcp_no_delay(uvTcp, enable ? 1 : 0).resolve();
	}

	/**
		Enable / disable TCP keep-alive.

		`delay` is the initial delay in seconds.
	**/
	public function keepAlive(enable:Bool, delay:UInt) {
		UV.tcp_keepalive(uvTcp, enable ? 1 : 0, delay).resolve();
	}

	/**
		Enable / disable simultaneous asynchronous accept requests that are queued
		by the operating system when listening for new TCP connections.
	**/
	public function simultaneousAccepts(enable:Bool) {
		UV.tcp_simultaneous_accepts(uvTcp, enable ? 1 : 0).resolve();
	}

	/**
		This call is used in conjunction with `Tcp.listen(backlog, callback)` to
		accept incoming connections.

		Call this function after receiving a `callback` to accept the connection.
	**/
	public function bind(addr:SockAddr, ipv6Only = false) {
		UV.bind(uvTcp, addr.uvAddr, ipv6Only ? UV_TCP_IPV6ONLY : 0).resolve();
	}

	/**
		This call is used in conjunction with `tcp.listen(backlog, callback)` to
		accept incoming connections.

		Call this function after receiving a `callback` to accept the connection.
	**/
	public function accept(client:Tcp) {
		UV.accept(uvTcp, client.uvTcp).resolve();
	}

}