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
package neko.net;

class ThreadRemotingServer extends ThreadServer<haxe.remoting.SocketConnection,String> {

	var domains : Array<String>;
	var port : Int;

	public function new( ?domains ) {
		super();
		messageHeaderSize = 2;
		this.domains = domains;
	}

	public dynamic function initClientApi( cnx : haxe.remoting.SocketConnection, ctx : haxe.remoting.Context ) {
		throw "Not implemented";
	}

	public dynamic function onXml( cnx : haxe.remoting.SocketConnection, data : String ) {
		throw "Unhandled XML data '"+data+"'";
	}

	public dynamic function makePolicyFile() {
		var str = "<cross-domain-policy>";
		for( d in domains )
			str += '<allow-access-from domain="'+d+'" to-ports="'+port+'"/>';
		str += "</cross-domain-policy>";
		return str;
	}

	public override function run( host, port ) {
		this.port = port;
		super.run(host,port);
	}

	public override function clientConnected( s : sys.net.Socket ) {
		var ctx = new haxe.remoting.Context();
		var cnx = haxe.remoting.SocketConnection.create(s,ctx);
		var me = this;
		cnx.setErrorHandler(function(e) {
			if( !Std.is(e,haxe.io.Eof) && !Std.is(e,haxe.io.Error) )
				me.logError(e);
			me.stopClient(s);
		});
		initClientApi(cnx,ctx);
		return cnx;
	}

	override function readClientMessage( cnx : haxe.remoting.SocketConnection, buf : haxe.io.Bytes, pos : Int, len : Int ) {
		var msgLen = cnx.getProtocol().messageLength(buf.get(pos),buf.get(pos+1));
		if( msgLen == null ) {
			if( buf.get(pos) != 60 )
				throw "Invalid remoting message '"+buf.getString(pos,len)+"'";
			var p = pos;
			while( p < len ) {
				if( buf.get(p) == 0 )
					break;
				p++;
			}
			if( p == len )
				return null;
			p -= pos;
			return {
				msg : buf.getString(pos,p),
				bytes : p + 1,
			};
		}
		if( len < msgLen )
			return null;
		if( buf.get(pos + msgLen-1) != 0 )
			throw "Truncated message";
		return {
			msg : buf.getString(pos+2,msgLen-3),
			bytes : msgLen,
		};
	}

	public override function clientMessage( cnx : haxe.remoting.SocketConnection, msg : String ) {
		try {
			if( msg.charCodeAt(0) == 60 ) {
				if( domains != null && msg == "<policy-file-request/>" )
					cnx.getProtocol().socket.write(makePolicyFile()+"\x00");
				else
					onXml(cnx,msg);
			} else
				cnx.processMessage(msg);
		} catch( e : Dynamic ) {
			if( !Std.is(e,haxe.io.Eof) && !Std.is(e,haxe.io.Error) )
				logError(e);
			stopClient(cnx.getProtocol().socket);
		}
	}

}
