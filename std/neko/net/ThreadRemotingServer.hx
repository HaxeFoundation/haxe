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

	public function initClientApi( cnx, server ) {
		throw "Not implemented";
	}

	function decodeChar(c) {
		return haxe.remoting.SocketConnection.decodeChar(c);
	}

	public override function clientConnected( s : neko.net.Socket ) {
		var r = new neko.net.RemotingServer();
		var cnx = haxe.remoting.SocketConnection.socketConnect(s,r);
		var me = this;
		cnx.onError = function(e) {
			if( !Std.is(e,neko.io.Eof) && !Std.is(e,neko.io.Error) )
				me.logError(e);
			me.stopClient(s);
		};
		initClientApi(cnx,r);
		return cnx;
	}

	public override function readClientMessage( cnx, buf : String, pos : Int, len : Int ) {
		var c1 = decodeChar(buf.charCodeAt(pos));
		var c2 = decodeChar(buf.charCodeAt(pos+1));
		if( c1 == null || c2 == null ) {
			if( buf.charCodeAt(pos) != 60 )
				throw "Invalid remoting message '"+buf.substr(pos,len)+"'";
			// XML handling
			var p = buf.indexOf("\\0",pos);
			if( p == -1 )
				return null;
			return {
				msg : buf.substr(pos,p-pos),
				bytes : p - pos + 1,
			};
		}
		var msgLen = (c1 << 6) | c2;
		if( len < msgLen )
			return null;
		if( buf.charCodeAt(pos + msgLen-1) != 0 )
			throw "Truncated message";
		return {
			msg : buf.substr(pos+2,msgLen-3),
			bytes : msgLen,
		};
	}

	public function onXml(cnx,data) {
		throw "Unhandled XML data '"+data+"'";
	}

	public override function clientMessage( cnx, msg : String ) {
		if( msg.charCodeAt(0) == 60 ) {
			onXml(cnx,msg);
			return;
		}
		var r = haxe.remoting.SocketConnection.processMessage(cnx,msg);
		if( r != null ) {
			if( !Std.is(r.exc,neko.io.Eof) && !Std.is(r.exc,neko.io.Error) )
				logError(r.exc);
			stopClient(haxe.remoting.SocketConnection.getSocket(cnx));
		}
	}

}
