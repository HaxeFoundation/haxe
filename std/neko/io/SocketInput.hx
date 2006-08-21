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
package neko.io;
import neko.io.Socket;

class SocketInput extends Input {

	var __s : SocketHandle;

	public function new(s) {
		__s = s;
	}

	public override function readChar() {
		return try {
			socket_recv_char(__s);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Error.Blocked;
			else
				throw Error.Eof; // might also be closed socket
		}
	}

	public override function readBytes( buf : String, pos : Int, len : Int ) : Int {
		return try {
			socket_recv(__s,untyped buf.__s,pos,len);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Error.Blocked;
			else
				throw Error.Eof; // might also be closed socket or invalid param...
		}
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close(__s);
	}

	private static var socket_recv = neko.Lib.load("std","socket_recv",4);
	private static var socket_recv_char = neko.Lib.load("std","socket_recv_char",1);
	private static var socket_close = neko.Lib.load("std","socket_close",1);

}
