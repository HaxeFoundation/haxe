/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package neko.net;
import neko.net.Socket;
import neko.io.Error;

class SocketBufferedOutput extends neko.io.Output {

	var __s : SocketHandle;
	var buf : String;
	var bytes : Int;

	public function new( sock : neko.net.Socket, bufsize ) {
		var s : { private var __s : SocketHandle; } = sock;
		__s = s.__s;
		buf = neko.Lib.makeString(bufsize);
		bytes = 0;
	}

	public function reset() {
		bytes = 0;
	}

	public function writable() {
		return bytes == 0;
	}

	public override function flush() {
		if( bytes == 0 )
			return;
		var pos = 0;
		var len = bytes;
		var error;
		try {
			while( len > 0 ) {
				var k = socket_send(__s,untyped buf.__s,pos,len);
				pos += k;
				len -= k;
			}
		} catch( e : Dynamic ) {
			if( e != "Blocking" )
				error = { r : e };
		}
		if( pos > 0 ) {
			if( len == 0 )
				bytes = 0;
			else {
				untyped __dollar__sblit(buf.__s,0,buf.__s,pos,len);
				bytes = len;
			}
		}
		if( error != null )
			throw Custom(error.r);
	}

	public override function writeChar( c : Int ) {
		try {
			socket_send_char(__s, c);
		} catch( e : Dynamic ) {
			if( e == "Blocking" ) {
				untyped __dollar__sset(buf.__s,bytes,c);
				bytes += 1;
			} else
				throw Custom(e);
		}
	}

	public override function writeBytes( buf : String, pos : Int, len : Int) : Int {
		return try {
			socket_send(__s, untyped buf.__s, pos, len);
		} catch( e : Dynamic ) {
			if( e == "Blocking" ) {
				untyped __dollar__sblit(this.buf.__s,bytes,buf.__s,pos,len);
				bytes += len;
			} else
				throw Custom(e);
		}
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close(__s);
	}

	private static var socket_close = neko.Lib.load("std","socket_close",1);
	private static var socket_send_char = neko.Lib.load("std","socket_send_char",2);
	private static var socket_send = neko.Lib.load("std","socket_send",4);

}
