/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package sys.net;

/**
	An address is used to represent a port on a given host ip. 
	It is used by `sys.net.UdpSocket`.
**/
class Address {
	public var host : Int;
	public var port : Int;
	public function new() {
		host = 0;
		port = 0;
	}

	public function getHost() {
		var h = new Host("127.0.0.1");
		untyped h.ip = host;
		return h;
	}

	public function compare( a : Address ) {
		var dh = a.host - host;
		if( dh != 0 ) return dh;
		var dp = a.port - port;
		if( dp != 0 ) return dp;
		return 0;
	}

	public function clone() {
		var c = new Address();
		c.host = host;
		c.port = port;
		return c;
	}

}