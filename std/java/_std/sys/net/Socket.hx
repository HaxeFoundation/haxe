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
import java.net.InetSocketAddress;

@:coreApi
class Socket {

	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;

	public var custom : Dynamic;

	private var sock:java.net.Socket;
	private var server:java.net.ServerSocket;
	private var boundAddr:java.net.SocketAddress;

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

	public function read() : String
	{
		return input.readAll().toString();
	}

	public function write( content : String ) : Void
	{
		output.writeString(content);
	}

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

	public function listen( connections : Int ) : Void
	{
		if (boundAddr == null) throw "You must bind the Socket to an address!";
		try
			server.bind(boundAddr,connections)
		catch(e:Dynamic) throw e;
	}

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

	public function bind( host : Host, port : Int ) : Void
	{
		if (boundAddr != null)
		{
			if (server.isBound()) throw "Already bound";
		}
		this.boundAddr = new java.net.InetSocketAddress(host.wrapped, port);
	}

	public function accept() : Socket
	{
		var ret = try server.accept() catch(e:Dynamic) throw e;

		var s = new Socket();
		s.sock = ret;
		s.output = new java.io.NativeOutput(ret.getOutputStream());
		s.input = new java.io.NativeInput(ret.getInputStream());

		return s;
	}

	public function peer() : { host : Host, port : Int }
	{
		var rem = sock.getInetAddress();
		if (rem == null) return null;

		var host = new Host(null);
		host.wrapped = rem;
		return { host: host, port: sock.getPort() };
	}

	public function host() : { host : Host, port : Int }
	{
		var local = sock.getLocalAddress();
		var host = new Host(null);
		host.wrapped = local;
 
		if (boundAddr != null)
		{
			return { host: host, port: server.getLocalPort() };
		}

		return { host: host, port: sock.getLocalPort() };
	}

	public function setTimeout( timeout : Float ) : Void
	{
		try
			sock.setSoTimeout( Std.int(timeout * 1000) )
		catch(e:Dynamic) throw e;
	}

	public function waitForRead() : Void
	{
		throw "Not implemented";
	}

	public function setBlocking( b : Bool ) : Void
	{
		throw "Not implemented";
	}

	public function setFastSend( b : Bool ) : Void
	{
		try
			sock.setTcpNoDelay(b)
		catch(e:Dynamic) throw e;
	}

	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> }
	{
		throw "Not implemented";
		return null;
	}
}
