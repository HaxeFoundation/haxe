/*
 * Copyright (C)2005-2018 Haxe Foundation
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

import lua.lib.luasocket.Socket as LuaSocket;
import lua.lib.luasocket.socket.*;
import lua.*;

import haxe.io.Bytes;

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/

class Socket {

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
	var custom : Dynamic;

	/**
	  A Lua specific datatype for a tcp server instance
	**/
	var _socket : LuaSocket;


	var blocking = false;

	/**
		Creates a new unconnected socket.
	**/
	public function new() : Void { }

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	public function close() : Void {
		_socket.close();
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
		output.writeString(content);
	}

	/**
		Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
	**/
	public function connect( host : Host, port : Int ) : Void {
		var res = LuaSocket.connect(host.host, port);
		if (res.message != null) throw 'Socket Error : ${res.message}';
		input = new SocketInput(res.result);
		output = new SocketOutput(res.result);
		_socket = res.result;
	}

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
	**/
	public function listen( connections : Int ) : Void {
		var res = LuaSocket.tcp();
		if (res.message != null) throw 'Socket Listen Error : ${res.message}';
		res.result.listen(connections);
		_socket = res.result;
	}

	/**
		Shutdown the socket, either for reading or writing.
	**/
	public function shutdown( read : Bool, write : Bool ) : Void {
		var client : TcpClient = cast _socket;
		switch [read, write] {
			case [true  ,true]  : client.shutdown(Both);
			case [true  ,false] : client.shutdown(Receive);
			case [false ,true]  : client.shutdown(Send);
			default : null;
		}
	}

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	public function bind( host : Host, port : Int ) : Void {
		var res = LuaSocket.bind(host.host, port);
		if (res.message != null) throw 'Socket Bind Error : ${res.message}';
		_socket = res.result;
	}

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data.
	**/
	public function accept() : Socket {
		var server : TcpServer = cast _socket;
		var res = server.accept();
		if (res.message != null) throw 'Error : ${res.message}';
		var sock = new Socket();
		sock._socket = res.result;
		sock.input  = new SocketInput(res.result);
		sock.output = new SocketOutput(res.result);
		return sock;
	}

	/**
		Return the information about the other side of a connected socket.
	**/
	public function peer() : { host : Host, port : Int } {
		var client : TcpClient = cast _socket;
		var res = client.getpeername();
		var host = new Host(res.address);
		return  { host : host, port : Std.parseInt(res.port)};
	}

	/**
		Return the information about our side of a connected socket.
	**/
	public function host() : { host : Host, port : Int } {
		var server : TcpServer = cast _socket;
		var res = server.getsockname();
		var host = new Host(res.address);
		return {host : host, port : Std.parseInt(res.port)};
	}

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	public inline function setTimeout( timeout : Float ) : Void {
		var client : TcpClient = cast _socket;
		client.settimeout(timeout);
	}

	/**
		Block until some data is available for read on the socket.
	**/
	public function waitForRead() : Void {
		select([this], null, null);
	}

	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocked value.
	**/
	public function setBlocking( b : Bool ) : Void {
		blocking = b;
	}

	/**
		Allows the socket to immediately send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	public function setFastSend( b : Bool ) : Void {
		var client : TcpClient = cast _socket;
		client.setoption(TcpNoDelay, true);
	}

	/**
		Wait until one of the sockets groups is ready for the given operation :
		 - `read`contains sockets on which we want to wait for available data to be read,
		 - `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		 - `others` contains sockets on which we want to wait for exceptional conditions.
		 - `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	static public function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>, write: Array<Socket>, others: Array<Socket> } {
		var read_tbl  = read == null ? Table.create()   : Table.fromArray([for (r in read) cast r._socket]);
		var write_tbl  = write == null ? Table.create()  : Table.fromArray(([for (r in write) cast r._socket]));
		var res = LuaSocket.select(read_tbl, write_tbl, timeout);

		var convert_socket = function(x : LuaSocket){
			var sock = new Socket();
			sock.input = new SocketInput(cast x);
			sock.output = new SocketOutput(cast x);
			return sock;
		}
		var read_arr =  res.read == null ? [] : lua.Lib.tableToArray(res.read).map(convert_socket);

		var write_arr = res.write == null ? [] : lua.Lib.tableToArray(res.write).map(convert_socket);
		return {read : read_arr, write : write_arr, others : []};
	}

}

private class SocketInput extends haxe.io.Input {
	var s : TcpClient;
	public function new(s : TcpClient) {
		this.s = s;
	}
	override public function readByte() : Int {
		var res =  s.receive(1);
		if (res.message == "closed") throw new haxe.io.Eof();
		else if (res.message != null) throw 'Error : ${res.message}';
		return res.result.charCodeAt(0);
	}

	override public function readBytes( s : Bytes, pos : Int, len : Int ) : Int {
		var k = len;
		var b =  s.getData();
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;
		try{
			while( k > 0 ) {
				b[pos] = cast readByte();
				pos++;
				k--;
			}
		} catch(e:haxe.io.Eof){
			if (pos == 0){
				throw e;
			}
		}
		return len-k;
	}
}


private class SocketOutput extends haxe.io.Output {
	var s : TcpClient;
	public function new(s  : TcpClient) {
		this.s = s;
	}
	override public function writeByte(c : Int ) : Void {
		s.send(String.fromCharCode(c));
	}

}

