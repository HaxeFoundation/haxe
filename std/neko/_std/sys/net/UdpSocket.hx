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
import haxe.io.Error;

@:coreApi
class UdpSocket extends Socket {

	private override function init() : Void {
		__s = Socket.socket_new(true);
		super.init();
	}

	public function sendTo( buf : haxe.io.Bytes, pos : Int, len : Int, addr : Address ) : Int {
		return try {
			socket_send_to(__s, buf.getData(), pos, len, addr);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
	}

	public function readFrom( buf : haxe.io.Bytes, pos : Int, len : Int, addr : Address ) : Int {
		var r;
		try {
			r = socket_recv_from(__s,buf.getData(),pos,len,addr);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
		if( r == 0 )
			throw new haxe.io.Eof();
		return r;
	}

	static var socket_recv_from = neko.Lib.loadLazy("std", "socket_recv_from", 5);
	static var socket_send_to = neko.Lib.loadLazy("std", "socket_send_to", 5);

}
