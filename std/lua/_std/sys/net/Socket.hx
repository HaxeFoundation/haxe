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

#if lua_vanilla

class Socket {
	public var input(default, null):haxe.io.Input;
	public var output(default, null):haxe.io.Output;

	static inline function notImplemented():Dynamic
		throw new haxe.exceptions.NotImplementedException("Socket cannot be used with -D lua-vanilla because it requires luasocket");

	public function new():Void notImplemented();
	public function close():Void notImplemented();
	public function read():String return notImplemented();
	public function write(content:String):Void notImplemented();
	public function connect(host:Host, port:Int):Void notImplemented();
	public function listen(connections:Int):Void notImplemented();
	public function shutdown(read:Bool, write:Bool):Void notImplemented();
	public function bind(host:Host, port:Int):Void notImplemented();
	public function accept():Socket return notImplemented();
	public function peer():{host:Host, port:Int} return notImplemented();
	public function host():{host:Host, port:Int} return notImplemented();
	public function setTimeout(timeout:Float):Void notImplemented();
	public function waitForRead():Void notImplemented();
	public function setBlocking(b:Bool):Void notImplemented();
	public function setFastSend(b:Bool):Void notImplemented();
	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} return notImplemented();
}

#else

import lua.lib.luasocket.Socket as LuaSocket;
import lua.lib.luasocket.socket.*;
import lua.*;

import haxe.io.Bytes;
import haxe.io.Error;

// Can't enable @:coreApi because field 'custom' has different visibility (extern fields default to public, but class fields don't)
@:coreApi(check = Off)
class Socket {
	public var input(default, null):haxe.io.Input;

	public var output(default, null):haxe.io.Output;

	var custom:Dynamic;

	var _socket:LuaSocket;

	var blocking = false;
	var timeout = null;

	public function new():Void {}

	private function getTcpMaster():TcpMaster {
		if (_socket == null) {
			final res = LuaSocket.tcp();
			if (res.message != null)
				throw 'Socket Error : ${res.message}';
			_socket = res.result;
		}
		return cast _socket;
	}

	public function close():Void {
		_socket.close();
	}

	public function read():String {
		return input.readAll().toString();
	}

	public function write(content:String):Void {
		output.writeString(content);
	}

	public function connect(host:Host, port:Int):Void {
		final master = getTcpMaster();
		master.connect(host.host, port);
		final client:TcpClient = cast _socket;
		input = new SocketInput(client);
		output = new SocketOutput(client);
		_socket.settimeout(timeout);
	}

	public function listen(connections:Int):Void {
		final server:TcpMaster = getTcpMaster();
		server.listen(connections);
		_socket.settimeout(timeout);
	}

	public function shutdown(read:Bool, write:Bool):Void {
		var client:TcpClient = cast _socket;
		switch [read, write] {
			case [true, true]:
				client.shutdown(Both);
			case [true, false]:
				client.shutdown(Receive);
			case [false, true]:
				client.shutdown(Send);
			default:
				null;
		}
	}

	public function bind(host:Host, port:Int):Void {
		final master = getTcpMaster();
		master.bind(host.host, port);
	}

	public function accept():Socket {
		var server:TcpServer = cast _socket;
		var res = server.accept();
		if (res.message != null)
			throw 'Error : ${res.message}';
		var sock = new Socket();
		sock._socket = res.result;
		sock.input = new SocketInput(res.result);
		sock.output = new SocketOutput(res.result);
		return sock;
	}

	public function peer():{host:Host, port:Int} {
		var client:TcpClient = cast _socket;
		var res = client.getpeername();
		var host = new Host(res.address);
		return {host: host, port: Std.parseInt(res.port)};
	}

	public function host():{host:Host, port:Int} {
		var server:TcpServer = cast _socket;
		var res = server.getsockname();
		var host = new Host(res.address);
		return {host: host, port: Std.parseInt(res.port)};
	}

	public inline function setTimeout(timeout:Float):Void {
		this.timeout = timeout;
		if (_socket != null) {
			var client:TcpClient = cast _socket;
			client.settimeout(timeout);
		}
	}

	public function waitForRead():Void {
		select([this], null, null);
	}

	public function setBlocking(b:Bool):Void {
		blocking = b;
	}

	public function setFastSend(b:Bool):Void {
		var client:TcpClient = cast _socket;
		client.setoption(TcpNoDelay, true);
	}

	static public function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var read_tbl = read == null ? Table.create() : Table.fromArray([for (r in read) cast r._socket]);
		var write_tbl = write == null ? Table.create() : Table.fromArray(([for (r in write) cast r._socket]));
		var res = LuaSocket.select(read_tbl, write_tbl, timeout);

		var convert_socket = function(x:LuaSocket) {
			var sock = new Socket();
			sock.input = new SocketInput(cast x);
			sock.output = new SocketOutput(cast x);
			return sock;
		}
		var read_arr = res.read == null ? [] : Table.toArray(res.read).map(convert_socket);

		var write_arr = res.write == null ? [] : Table.toArray(res.write).map(convert_socket);
		return {read: read_arr, write: write_arr, others: []};
	}
}

#end
