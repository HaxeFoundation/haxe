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

#if cs
package cs.net;

import sys.net.Host;

class SslSocket extends sys.net.Socket {
	override public function connect(host:Host, port:Int):Void {
		var hostName:String = host.host;
		var ipStr:String = host.toString();

		// Connect the underlying TCP socket
		_socket.Connect(ipStr, port);
		var connected:Bool = untyped __cs__("{0}.Connected", _socket);
		if (!connected) {
			throw "Connection failed.";
		}

		// Wrap with NetworkStream, then SslStream, authenticate, and return as Stream
		// Using lambda to get proper typing in generated C#
		var sslStream:cs.system.io.Stream = untyped __cs__("((System.Func<System.IO.Stream>)(() => { var ns = new System.Net.Sockets.NetworkStream({0}); var ssl = new System.Net.Security.SslStream(ns, false); ssl.AuthenticateAsClient({1}); return ssl; }))()", _socket, hostName);

		// Create Haxe I/O from the SSL stream
		this.input = new cs.io.NativeInput(sslStream);
		this.output = new cs.io.NativeOutput(sslStream);
	}
}
#end
