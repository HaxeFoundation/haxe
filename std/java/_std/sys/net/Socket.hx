/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import java.net.InetSocketAddress;

@:coreApi
class Socket {

	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;

	/**
		A custom value that can be associated with the socket. Can be used to retreive your custom infos after a [select].
	***/
	public var custom : Dynamic;

	private var sock:java.net.Socket;
	private var server:java.net.ServerSocket;
	private var boundAddr:java.net.SocketAddress;
	/**
		Creates a new unconnected socket.
	**/
	public function new() : Void
	{
		create();
	}

	private function create():Void
	{
		this.sock = new java.net.Socket();
		try
		{
			this.server = new java.net.ServerSocket();
		} catch(e:Dynamic) throw e;
	}

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	public function close() : Void
	{
		try
		{
			if (sock != null)
				this.sock.close();
			if (server != null)
				this.server.close();
		}
		catch(e:Dynamic) throw e;
	}

	/**
		Read the whole data available on the socket.
	**/
	public function read() : String
	{
		return input.readAll().toString();
	}

	/**
		Write the whole data to the socket output.
	**/
	public function write( content : String ) : Void
	{
		output.writeString(content);
	}

	/**
		Connect to the given server host/port. Throw an exception in case we couldn't sucessfully connect.
	**/
	public function connect( host : Host, port : Int ) : Void
	{
		try
		{
			sock.connect(new InetSocketAddress( host.wrapped, port));
			this.output = new java.io.NativeOutput(sock.getOutputStream());
			this.input = new java.io.NativeInput(sock.getInputStream());
		}
		catch(e:Dynamic) throw e;
	}

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use [accept()] to accept incoming connections.
	**/
	public function listen( connections : Int ) : Void
	{
		if (boundAddr == null) throw "You must bind the Socket to an address!";
		try
			server.bind(boundAddr,connections)
		catch(e:Dynamic) throw e;
	}

	/**
		Shutdown the socket, either for reading or writing.
	**/
	public function shutdown( read : Bool, write : Bool ) : Void
	{
		try
		{
			if (read)
				sock.shutdownInput();
			if (write)
				sock.shutdownOutput();
		}
		catch(e:Dynamic) throw e;
	}

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	public function bind( host : Host, port : Int ) : Void
	{
		if (boundAddr != null)
		{
			if (server.isBound()) throw "Already bound";
		}
		this.boundAddr = new java.net.InetSocketAddress(host.wrapped, port);
	}

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data.
	**/
	public function accept() : Socket
	{
		var ret = try server.accept() catch(e:Dynamic) throw e;

		var s = new Socket();
		s.sock = ret;

		return s;
	}

	/**
		Return the informations about the other side of a connected socket.
	**/
	public function peer() : { host : Host, port : Int }
	{
		var rem:java.net.InetSocketAddress = cast sock.getInetAddress();
		if (rem == null) return null;

		var host = new Host(null);
		host.wrapped = rem.getAddress();
		return { host: host, port: rem.getPort() };
	}

	/**
		Return the informations about our side of a connected socket.
	**/
	public function host() : { host : Host, port : Int }
	{
		var local = sock.getLocalAddress();
		var host = new Host(null);
		host.wrapped = local;

		return { host: host, port: sock.getPort() };
	}

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	public function setTimeout( timeout : Float ) : Void
	{
		try
			sock.setSoTimeout( Std.int(timeout * 1000) )
		catch(e:Dynamic) throw e;
	}

	/**
		Block until some data is available for read on the socket.
	**/
	public function waitForRead() : Void
	{
		throw "Not implemented";
	}

	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediatly by throwing a haxe.io.Error.Blocking value.
	**/
	public function setBlocking( b : Bool ) : Void
	{
		throw "Not implemented";
	}

	/**
		Allows the socket to immediatly send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	public function setFastSend( b : Bool ) : Void
	{
		try
			sock.setTcpNoDelay(b)
		catch(e:Dynamic) throw e;
	}

	/**
		Wait until one of the sockets groups is ready for the given operation :
		[read] contains sockets on which we want to wait for available data to be read,
		[write] contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		[others] contains sockets on which we want to wait for exceptional conditions.
		[select] will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a [timeout] (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> }
	{
		throw "Not implemented";
		return null;
	}

}
