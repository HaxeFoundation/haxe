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
import sys.net.Socket;

private typedef ServerClient<ClientData> = {
	var sock : Socket;
	var buffer : haxe.io.Bytes;
	var bufbytes : Int;
	var data : ClientData;
}

/**
	This class enables you to quickly create a custom server that can
	serve several clients in parallel. This server is using a single
	thread and process so the server itself processing is not parallel.
	Non-blocking sockets are used to ensure that a slow client does not
	block the others.
**/
class ServerLoop<ClientData> {

	/**
		Each client has an associated buffer. This is the initial buffer size which
		is set to 128 bytes by default.
	**/
	public static var DEFAULT_BUFSIZE = 128;

	/**
		Each client has an associated buffer. This is the maximum buffer size which
		is set to 64K by default. When that size is reached and some data can't be processed,
		the client is disconnected.
	**/
	public static var MAX_BUFSIZE = (1 << 16);

	/**
		This is the value of number client requests that the server socket
		listen for. By default this number is 10 but can be increased for
		servers supporting a large number of simultaneous requests.
	**/
	public var listenCount : Int;

	/**
		See `update`.
	**/
	public var updateTime : Float;

	var newData : Socket -> ClientData;
	var socks : Array<sys.net.Socket>;
	public var clients : List<ClientData>;

	/**
		Creates a server instance. The `newData` methods must return
		the data associated with the Client.
	**/
	public function new( ?newData ) {
		this.newData = if( newData == null ) function(_) { return null; } else newData;
		clients = new List();
		socks = new Array();
		listenCount = 10;
		updateTime = 1;
	}

	/**
		Closes the client connection and removes it from the client List.
	**/
	public function closeConnection( s : Socket ) : Bool {
		var cl : ServerClient<ClientData> = untyped s.__client;
		if( cl == null || !clients.remove(cl.data) )
			return false;
		socks.remove(s);
		try s.close() catch( e : Dynamic ) { };
		clientDisconnected(cl.data);
		return true;
	}

	/**
		The `update` method is called after each socket event has been
		processed or when `updateTime` has been reached. It can be used
		to perform time-regular tasks such as pings. By default `updateTime`
		is set to one second.
	**/
	public function update() {
	}

	/**
		This method is called after a client has been disconnected.
	**/
	public function clientDisconnected( d : ClientData ) {
	}

	/**
		This method can be used instead of writing directly to the socket.
		It ensures that all the data is correctly sent. If an error occurs
		while sending the data, no exception will occur but the client will
		be gracefully disconnected.
	**/
	public function clientWrite( s : Socket, buf : haxe.io.Bytes, pos : Int, len : Int ) {
		try {
			while( len > 0 ) {
				var nbytes = s.output.writeBytes(buf,pos,len);
				pos += nbytes;
				len -= nbytes;
			}
		} catch( e : Dynamic ) {
			closeConnection(s);
		}
	}

	/**
		This method is called when some data has been read into a Client buffer.
		If the data can be handled, then you can return the number of bytes handled
		that needs to be removed from the buffer. It the data can't be handled (some
		part of the message is missing for example), returns 0.
	**/
	public function processClientData( d : ClientData, buf : haxe.io.Bytes, bufpos : Int, buflen : Int ) {
		throw "ServerLoop::processClientData is not implemented";
		return 0;
	}

	/**
		Called when an error occurred. This enable you to log the error somewhere.
		By default the error is displayed using `trace`.
	**/
	public function onError( e : Dynamic ) {
		trace(Std.string(e)+"\n"+haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
	}

	function readData( cl : ServerClient<ClientData> ) {
		var buflen = cl.buffer.length;
		// eventually double the buffer size
		if( cl.bufbytes == buflen ) {
			var nsize = buflen * 2;
			if( nsize > MAX_BUFSIZE ) {
				if( buflen == MAX_BUFSIZE )
					throw "Max buffer size reached";
				nsize = MAX_BUFSIZE;
			}
			var buf2 = haxe.io.Bytes.alloc(nsize);
			buf2.blit(0,cl.buffer,0,buflen);
			buflen = nsize;
			cl.buffer = buf2;
		}
		// read the available data
		var nbytes = cl.sock.input.readBytes(cl.buffer,cl.bufbytes,buflen - cl.bufbytes);
		cl.bufbytes += nbytes;
	}

	function processData( cl : ServerClient<ClientData> ) {
		var pos = 0;
		while( cl.bufbytes > 0 ) {
			var nbytes = processClientData(cl.data,cl.buffer,pos,cl.bufbytes);
			if( nbytes == 0 )
				break;
			pos += nbytes;
			cl.bufbytes -= nbytes;
		}
		if( pos > 0 )
			cl.buffer.blit(0,cl.buffer,pos,cl.bufbytes);
	}

	/**
		Run the server. This function should never return.
	**/
	public function run( host : sys.net.Host, port : Int ) {
		var serv = new Socket();
		serv.bind(host,port);
		serv.listen(listenCount);
		socks = [serv];
		while( true ) {
			for( s in Socket.select(socks,null,null,updateTime).read ) {
				var cl : ServerClient<ClientData> = untyped s.__client;
				if( cl == null ) {
					// no associated client : it's our server socket
					var sock = serv.accept();
					sock.setBlocking(true);
					cl = {
						sock : sock,
						data : null,
						buffer : haxe.io.Bytes.alloc(DEFAULT_BUFSIZE),
						bufbytes : 0,
					};
					// bind the client
					untyped sock.__client = cl;
					// creates the data
					try {
						cl.data = newData(sock);
					} catch( e : Dynamic ) {
						onError(e);
						try sock.close() catch( e : Dynamic ) { };
						continue;
					}
					// adds the client to the lists
					socks.push(sock);
					clients.add(cl.data);
				} else {
					// read & process the data
					try {
						readData(cl);
						processData(cl);
					} catch( e : Dynamic ) {
						if( !Std.is(e,haxe.io.Eof) )
							onError(e);
						closeConnection(cl.sock);
					}
				}
			}
			update();
		}
		serv.close();
	}

}
