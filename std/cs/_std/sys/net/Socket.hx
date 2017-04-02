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

import cs.system.net.IPEndPoint;
import cs.system.net.sockets.AddressFamily;
import cs.system.net.sockets.NetworkStream;
import cs.system.net.sockets.ProtocolType;
import cs.system.net.sockets.SocketFlags;
import cs.system.net.sockets.SocketShutdown;
import cs.system.net.sockets.SocketType;
import cs.system.threading.Thread;
import haxe.io.Bytes;
import haxe.io.Error;
import haxe.io.Input;
import haxe.io.Output;

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
@:coreapi
class Socket {
	private var sock : cs.system.net.sockets.Socket = null;

	/**
		The stream on which you can read available data. By default the stream is blocking until the requested data is available,
		use `setBlocking(false)` or `setTimeout` to prevent infinite waiting.
	**/
	public var input(default,null) : haxe.io.Input;

	/**
		The stream on which you can send data. Please note that in case the output buffer you will block while writing the data, use `setBlocking(false)` or `setTimeout` to prevent that.
	**/
	public var output(default,null) : haxe.io.Output;

	/**
		A custom value that can be associated with the socket. Can be used to retrieve your custom infos after a `select`.
	***/
	public var custom : Dynamic;

	/**
		Creates a new unconnected socket.
	**/
	public function new() : Void {
		sock = new cs.system.net.sockets.Socket( AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp );
		sock.Blocking = true;
	}

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	public function close() : Void {
		sock.Close();
		input = null;
		output = null;
	}

	/**
		Read the whole data available on the socket.
	**/
	public function read() : String {
		return input.readAll().toString();
	}

	/**
		Write the whole data to the socket output.
	**/
	public function write( content : String ) : Void {
		output.writeString( content );
	}

	/**
		Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
	**/
	public function connect( host : Host, port : Int ) : Void {
		sock.Connect( host.ipAddress, port );
		if (sock.Connected) {
			this.output = new cs.io.NativeOutput( new NetworkStream(sock) );
			this.input = new cs.io.NativeInput( new NetworkStream(sock) );
		} else {
			throw "Connection failed.";
		}
	}

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
	**/
	public function listen( connections : Int ) : Void {
		sock.Listen( connections );
	}

	/**
		Shutdown the socket, either for reading or writing.
	**/
	public function shutdown( read : Bool, write : Bool ) : Void {
		if ( read && write ) {
			sock.Shutdown( SocketShutdown.Both );
			input = null;
			output = null;
		} else if ( read ) {
			sock.Shutdown( SocketShutdown.Receive );
			input = null;
		} else if ( write ) {
			sock.Shutdown( SocketShutdown.Send );
			output = null;
		}
	}

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	public function bind( host : Host, port : Int ) : Void {
		sock = new cs.system.net.sockets.Socket( AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp );
		sock.Bind( new IPEndPoint(host.ipAddress, port) );
	}

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data.
	**/
	public function accept() : Socket {
		var r = new Socket();
		r.sock = sock.Accept();
		r.output = new cs.io.NativeOutput( new NetworkStream(r.sock) );
		r.input = new cs.io.NativeInput( new NetworkStream(r.sock) );
		return r;
	}

	/**
		Return the information about the other side of a connected socket.
	**/
	public function peer() : { host : Host, port : Int } {
		var remoteIP = cast(sock.RemoteEndPoint, IPEndPoint);
		return { host: new Host(remoteIP.Address.ToString()), port: remoteIP.Port };
	}

	/**
		Return the information about our side of a connected socket.
	**/
	public function host() : { host : Host, port : Int } {
		var localIP = cast(sock.LocalEndPoint, IPEndPoint);
		return { host: new Host(localIP.Address.ToString()), port: localIP.Port };
	}

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	public function setTimeout( timeout : Float ) : Void {
		sock.ReceiveTimeout = sock.SendTimeout = Math.round(timeout * 1000);
	}

	/**
		Block until some data is available for read on the socket.
	**/
	public function waitForRead() : Void {
		var end = Date.now().getTime() + ((sock.ReceiveTimeout <= 0) ? Math.POSITIVE_INFINITY : sock.ReceiveTimeout);
		while ( sock.Available == 0 && Date.now().getTime() < end) {
			Thread.Sleep(5);
		}
	}

	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocking value.
	**/
	public function setBlocking( b : Bool ) : Void {
		sock.Blocking = b;
	}

	/**
		Allows the socket to immediately send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	public function setFastSend( b : Bool ) : Void {
		sock.NoDelay = b;
	}

	/**
		Wait until one of the sockets groups is ready for the given operation :
		* `read` contains sockets on which we want to wait for available data to be read,
		* `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		* `others` contains sockets on which we want to wait for exceptional conditions.
		* `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	static public function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> } {
		throw "Not implemented yet.";
		return null;
	}

}
