/*
 * Copyright (C)2005-2018 Haxe Foundation
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
import sys.net.Socket;
import haxe.io.Error;

class UdpSocket extends Socket {

	public function new() {
		super();
	}

	override function init() : Void {
		__s = Socket.socket_new(true);
		super.init();
	}
	
	public function sendTo( buf : haxe.io.Bytes, pos : Int, len : Int, addr : Address ) : Int {
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw OutsideBounds;
		return try {
			socket_send_to(__s, (buf:hl.Bytes).offset(pos), len, addr.host, addr.port);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
	}

	public function readFrom( buf : haxe.io.Bytes, pos : Int, len : Int, addr : Address ) : Int {
		var r;
		var host = 0, port = 0;
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw OutsideBounds;
		try {
			r = socket_recv_from(__s,(buf:hl.Bytes).offset(pos),len,host,port);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
		if( r == 0 )
			throw new haxe.io.Eof();
		addr.host = host;
		addr.port = port;
		return r;
	}

	@:hlNative("std","socket_send_to") static function socket_send_to( s : SocketHandle, bytes : hl.Bytes, len : Int, host : Int, port : Int ) : Int { return 0; }

	@:hlNative("std","socket_recv_from") static function socket_recv_from( s : SocketHandle, bytes : hl.Bytes, len : Int, host : hl.Ref<Int>, port : hl.Ref<Int> ) : Int { return 0; }
	
}
