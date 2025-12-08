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

package sys.net;

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
class AsyncSocket {

	/**
		Creates a new unconnected socket. Use the current event loop by default.
	**/
	public function new( ?loop : haxe.EventLoop ) {
		throw "Not implemented on this platform";
	}

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	public function close() {
	}

	/**
		Write the whole data to the socket output, asynchronously. The buffer can be reused after this call. Will dispatch `onWrite` when data is written.
	**/
	public function write( bytes : haxe.io.Bytes, pos : Int = 0, len : Int = -1 ) {
	}

	/**
		Connect to the given server host/port. Will onConnect if connection successful and onDisconnect if connection fails or if a later disconnection occurs.
		If `ssl` is set, the socket will use SSL authentification and encryption. Certificate errors can generate an onSSLError callback.
	**/
	public function connect(host:Host, port:Int, ssl = false) {
	}

	/**
		Allows the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused.
		Each new connection will trigger a `onConnected` call. Use `accept()` to retrieve the incoming connection socket.
	**/
	public function listen(connections:Int) {
	}

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	public function bind(host:Host, port:Int, ?ssl : { hostname : String, cert : sys.ssl.Certificate, key : sys.ssl.Key } ) {
	}

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data. See `listen`
	**/
	public function accept():Null<AsyncSocket> {
		return null;
	}

	/**
		Allows the socket to immediately send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	public function setFastSend(b:Bool) {
	}

	/**
		See `connect`
	**/
	public dynamic function onConnect() {
	}

	/**
		See `connect`
	**/
	public dynamic function onDisconnect() {
	}

	/**
		This callback is called when some data is received on the socket. The bytes must be fully processed in the callback as the data might be overwritten upon return.
	**/
	public dynamic function onData( bytes : haxe.io.Bytes, pos : Int, len : Int ) {
	}

	/**
		Dispatched when some data is written. Error is true if the data could not be written succesfully. The default behavior is to silently close the socket when such error occurs.
	**/
	public dynamic function onWrite( error : Bool ) {
		if( error ) {
			onDisconnect();
			close();
		}
	}
	
	/**
		Dispatched when a SSL error occurs. This can be when connecting to a host which fails to authentificate or when starting a server with invalid cert/key.
		Authentification can be adjusted with `sys.ssl.Socket.DEFAULT_VERIFY_CERT`
	**/
	public dynamic function onSSLError( msg : String ) {
	}

	/**
		Similar to `write` but sends a whole string data.
	**/
	public function writeString( str : String ) {
		var buf = haxe.io.Bytes.ofString(str);
		write(buf, 0, buf.length);
	}


	/**
		Start a server on given host/port and callback `onClient` everytime a client connects.
		Will also call `onClient(null)` in the rare case where the server connection is lost.
	**/
	public static function startServer( host, port, ?ssl, onClient ) {
		var s = new AsyncSocket();
		s.onDisconnect = function() {
			onClient(null);
		};
		s.onConnect = function() {
			onClient(s.accept());
		};
		s.bind(new Host(host), port, ssl);
		s.listen(1);
		return s;
	}

}
