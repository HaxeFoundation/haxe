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

class ThreadRemotingServer extends ThreadServer<haxe.remoting.SocketConnection,String> {

	public function new() {
		super();
		messageHeaderSize = 2;
	}

	public function newClientApi() {
		throw "Not implemented";
		return null;
	}

	function decodeChar(c) {
		// A...Z
		if( c >= 65 && c <= 90 )
			return c - 65;
		// a...z
		if( c >= 97 && c <= 122 )
			return c - 97 + 26;
		// 0...9
		if( c >= 48 && c <= 57 )
			return c - 48 + 52;
		// +
		if( c == 43 )
			return 62;
		// /
		if( c == 47 )
			return 63;
		return null;
	}

	public override function clientConnected( s : neko.net.Socket ) {
		return haxe.remoting.SocketConnection.socketConnect(s,newClientApi());
	}

	public override function readClientMessage( cnx, buf : String, pos : Int, len : Int ) {
		var c1 = decodeChar(buf.charCodeAt(pos));
		var c2 = decodeChar(buf.charCodeAt(pos+1));
		var msgLen = (c1 << 6) | c2;
		if( len < msgLen )
			return null;
		if( buf.charCodeAt(msgLen-1) != 0 )
			throw "Truncated message";
		return {
			msg : buf.substr(pos+2,msgLen-3),
			bytes : msgLen,
		};
	}

	public override function clientMessage( cnx, msg : String ) {
		var r = haxe.remoting.SocketConnection.processMessage(cnx,msg);
		if( r != null )
			neko.Lib.rethrow(r.exc);
	}

}
