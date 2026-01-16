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

@:coreApi
class Socket {
	public var input(default, null):haxe.io.Input;
	public var output(default, null):haxe.io.Output;

	public var custom:Dynamic;

	private var _socket:Dynamic; // System.Net.Sockets.Socket
	private var _server:Dynamic; // System.Net.Sockets.Socket (for listening)
	private var _boundHost:Host;
	private var _boundPort:Int;

	public function new():Void {
		create();
	}

	private function create():Void {
		// Create TCP socket: AddressFamily.InterNetwork = 2, SocketType.Stream = 1, ProtocolType.Tcp = 6
		_socket = untyped __cs__("new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp)");
	}

	public function close():Void {
		try {
			if (_socket != null) {
				untyped __cs__("{0}.Close()", _socket);
			}
			if (_server != null) {
				untyped __cs__("{0}.Close()", _server);
			}
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function read():String {
		return input.readAll().toString();
	}

	public function write(content:String):Void {
		output.writeString(content);
	}

	public function connect(host:Host, port:Int):Void {
		try {
			var ipStr:String = host.toString();
			untyped __cs__("{0}.Connect({1}, {2})", _socket, ipStr, port);
			setupStreams();
		} catch (e:Dynamic) {
			throw e;
		}
	}

	private function setupStreams():Void {
		var netStream:Dynamic = untyped __cs__("new System.Net.Sockets.NetworkStream({0})", _socket);
		this.input = new cs.io.NativeInput(netStream);
		this.output = new cs.io.NativeOutput(netStream);
	}

	public function listen(connections:Int):Void {
		if (_server == null) {
			throw "You must bind the Socket to an address!";
		}
		try {
			untyped __cs__("{0}.Listen({1})", _server, connections);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function shutdown(read:Bool, write:Bool):Void {
		try {
			if (read && write) {
				untyped __cs__("{0}.Shutdown(System.Net.Sockets.SocketShutdown.Both)", _socket);
			} else if (read) {
				untyped __cs__("{0}.Shutdown(System.Net.Sockets.SocketShutdown.Receive)", _socket);
			} else if (write) {
				untyped __cs__("{0}.Shutdown(System.Net.Sockets.SocketShutdown.Send)", _socket);
			}
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function bind(host:Host, port:Int):Void {
		if (_server != null) {
			throw "Already bound";
		}
		_boundHost = host;
		_boundPort = port;
		// Create server socket
		_server = untyped __cs__("new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp)");
		try {
			var ipStr:String = host.toString();
			untyped __cs__("{0}.Bind(new System.Net.IPEndPoint(System.Net.IPAddress.Parse({1}), {2}))", _server, ipStr, port);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function accept():Socket {
		try {
			var clientSocket:Dynamic = untyped __cs__("{0}.Accept()", _server);

			var s = new Socket();
			s._socket = clientSocket;
			s.setupStreams();

			return s;
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function peer():{host:Host, port:Int} {
		try {
			var remoteEp:Dynamic = untyped __cs__("{0}.RemoteEndPoint as System.Net.IPEndPoint", _socket);
			if (remoteEp == null) {
				return null;
			}

			var ipStr:String = untyped __cs__("{0}.Address.ToString()", remoteEp);
			var port:Int = untyped __cs__("{0}.Port", remoteEp);
			var host = new Host(ipStr);
			return {host: host, port: port};
		} catch (e:Dynamic) {
			return null;
		}
	}

	public function host():{host:Host, port:Int} {
		try {
			var localEp:Dynamic;
			if (_server != null) {
				localEp = untyped __cs__("{0}.LocalEndPoint as System.Net.IPEndPoint", _server);
			} else {
				localEp = untyped __cs__("{0}.LocalEndPoint as System.Net.IPEndPoint", _socket);
			}

			if (localEp == null) {
				return null;
			}

			var ipStr:String = untyped __cs__("{0}.Address.ToString()", localEp);
			var port:Int = untyped __cs__("{0}.Port", localEp);
			var host = new Host(ipStr);
			return {host: host, port: port};
		} catch (e:Dynamic) {
			return null;
		}
	}

	public function setTimeout(timeout:Float):Void {
		try {
			var timeoutMs:Int = Std.int(timeout * 1000);
			untyped __cs__("{0}.ReceiveTimeout = {1}", _socket, timeoutMs);
			untyped __cs__("{0}.SendTimeout = {1}", _socket, timeoutMs);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function waitForRead():Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function setBlocking(b:Bool):Void {
		try {
			untyped __cs__("{0}.Blocking = {1}", _socket, b);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public function setFastSend(b:Bool):Void {
		try {
			untyped __cs__("{0}.NoDelay = {1}", _socket, b);
		} catch (e:Dynamic) {
			throw e;
		}
	}

	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		throw new haxe.exceptions.NotImplementedException();
		return null;
	}
}
