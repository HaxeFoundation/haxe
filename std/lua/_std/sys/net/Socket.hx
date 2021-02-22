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

import haxe.io.Bytes;
import haxe.io.Error;
import lua.*;
import lua.lib.luasocket.Socket as LuaSocket;
import lua.lib.luasocket.socket.*;

@:nullSafety(Off)
class Socket {
	public var input(default, null):haxe.io.Input;

	public var output(default, null):haxe.io.Output;

	var custom:Dynamic;

	var _socket:LuaSocket;

	var blocking = false;
	var timeout = null;

	public function new():Void {}

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
		var res = LuaSocket.connect(host.host, port);
		if (res.message != null)
			throw 'Socket Error : ${res.message}';
		input = new SocketInput(res.result);
		output = new SocketOutput(res.result);
		_socket = res.result;
		_socket.settimeout(timeout);
	}

	public function listen(connections:Int):Void {
		var res = LuaSocket.tcp();
		if (res.message != null)
			throw 'Socket Listen Error : ${res.message}';
		res.result.listen(connections);
		_socket = res.result;
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
		var res = LuaSocket.bind(host.host, port);
		if (res.message != null)
			throw 'Socket Bind Error : ${res.message}';
		_socket = res.result;
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

private class SocketInput extends haxe.io.Input {
	var tcp:TcpClient;

	public function new(tcp:TcpClient) {
		this.tcp = tcp;
	}

	override public function readByte():Int {
		var res = tcp.receive(1);
		if (res.message == "closed") {
			throw new haxe.io.Eof();
		} else if (res.message != null)
			throw 'Error : ${res.message}';
		return res.result.charCodeAt(0);
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var leftToRead = len;
		var b = s.getData();
		if (pos < 0 || len < 0 || pos + len > s.length)
			throw haxe.io.Error.OutsideBounds;
		var readCount = 0;
		try {
			while (leftToRead > 0) {
				b[pos] = cast readByte();
				pos++;
				readCount++;
				leftToRead--;
			}
		} catch (e:haxe.io.Eof) {
			if (readCount == 0) {
				throw e;
			}
		}
		return readCount;
	}
}

private class SocketOutput extends haxe.io.Output {
	var tcp:TcpClient;

	public function new(tcp:TcpClient) {
		this.tcp = tcp;
	}

	override public function writeByte(c:Int):Void {
		var char = NativeStringTools.char(c);
		var res = tcp.send(char);
		if (res.message != null) {
			throw 'Error : Socket writeByte : ${res.message}';
		}
	}

	override public function writeBytes(s:Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || pos + len > s.length)
			throw Error.OutsideBounds;
		var b = s.getData().slice(pos, pos + len).map(function(byte) {
			return lua.NativeStringTools.char(byte);
		});
		var encoded = Table.concat(cast b, 0);
		var res = tcp.send(encoded);
		if (res.message != null) {
			throw 'Error : Socket writeByte : ${res.message}';
		}

		return len;
	}
}
