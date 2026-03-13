/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

import js.node.dgram.Socket;

/**
	Datagram sockets
**/
@:jsRequire("dgram")
extern class Dgram {
	/**
		Creates a datagram `Socket` of the specified types.

		Takes an optional `callback` which is added as a listener for 'message' events.

		Call `socket.bind` if you want to receive datagrams. `socket.bind` will bind to
		the "all interfaces" address on a random port (it does the right thing for both `udp4` and `udp6` sockets).
		You can then retrieve the address and port with `socket.address().address` and `socket.address().port`.
	**/
	@:overload(function(type:SocketType, ?callback:MessageListener):Socket {})
	static function createSocket(options:SocketOptions, ?callback:MessageListener):Socket;
}
