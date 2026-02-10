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

import cs.system.net.sockets.NetworkStream;
import cs.system.net.sockets.Socket as NativeSocket;

@:coreApi
class Socket {
	public var input(default, null):haxe.io.Input;
	public var output(default, null):haxe.io.Output;

	public var custom:Dynamic;

	@:allow(cs.net.SslSocket)
	private var _socket:NativeSocket;

	public function new():Void {
		init();
	}

	private function init():Void {
		// Create TCP socket: AddressFamily.InterNetwork = 2, SocketType.Stream = 1, ProtocolType.Tcp = 6
		_socket = cs.Syntax.code("new global::System.Net.Sockets.Socket((global::System.Net.Sockets.AddressFamily)2, (global::System.Net.Sockets.SocketType)1, (global::System.Net.Sockets.ProtocolType)6)");
		_socket.Blocking = true;
	}

	public function close():Void {
		_socket.Close();
		input = null;
		output = null;
	}

	public function read():String {
		return input.readAll().toString();
	}

	public function write(content:String):Void {
		output.writeString(content);
	}

	public function connect(host:Host, port:Int):Void {
		var ipStr:String = host.toString();
		_socket.Connect(ipStr, port);
		var connected:Bool = cs.Syntax.code("{0}.Connected", _socket);
		if (connected) {
			var netStream = new NetworkStream(_socket);
			this.input = new cs.io.NativeInput(netStream);
			this.output = new cs.io.NativeOutput(netStream);
		} else {
			throw "Connection failed.";
		}
	}

	public function listen(connections:Int):Void {
		_socket.Listen(connections);
	}

	public function shutdown(read:Bool, write:Bool):Void {
		if (read && write) {
			cs.Syntax.code("{0}.Shutdown((global::System.Net.Sockets.SocketShutdown)2)", _socket); // Both
			input = null;
			output = null;
		} else if (read) {
			cs.Syntax.code("{0}.Shutdown((global::System.Net.Sockets.SocketShutdown)0)", _socket); // Receive
			input = null;
		} else if (write) {
			cs.Syntax.code("{0}.Shutdown((global::System.Net.Sockets.SocketShutdown)1)", _socket); // Send
			output = null;
		}
	}

	public function bind(host:Host, port:Int):Void {
		// Create a new socket for binding (like Haxe4 does)
		_socket = cs.Syntax.code("new global::System.Net.Sockets.Socket((global::System.Net.Sockets.AddressFamily)2, (global::System.Net.Sockets.SocketType)1, (global::System.Net.Sockets.ProtocolType)6)");
		var ipStr:String = host.toString();
		cs.Syntax.code("{0}.Bind(new global::System.Net.IPEndPoint(global::System.Net.IPAddress.Parse({1}), {2}))", _socket, ipStr, port);
	}

	public function accept():Socket {
		var clientSocket:NativeSocket = _socket.Accept();
		var s = new Socket();
		s._socket = clientSocket;
		var netStream = new NetworkStream(s._socket);
		s.input = new cs.io.NativeInput(netStream);
		s.output = new cs.io.NativeOutput(netStream);
		return s;
	}

	public function peer():{host:Host, port:Int} {
		var remoteEp:Dynamic = _socket.RemoteEndPoint;
		if (remoteEp == null) {
			return null;
		}
		var ipStr:String = cs.Syntax.code("((global::System.Net.IPEndPoint){0}).Address.ToString()", remoteEp);
		var port:Int = cs.Syntax.code("((global::System.Net.IPEndPoint){0}).Port", remoteEp);
		var host = new Host(ipStr);
		return {host: host, port: port};
	}

	public function host():{host:Host, port:Int} {
		var localEp:Dynamic = _socket.LocalEndPoint;
		if (localEp == null) {
			return null;
		}
		var ipStr:String = cs.Syntax.code("((global::System.Net.IPEndPoint){0}).Address.ToString()", localEp);
		var port:Int = cs.Syntax.code("((global::System.Net.IPEndPoint){0}).Port", localEp);
		var host = new Host(ipStr);
		return {host: host, port: port};
	}

	public function setTimeout(timeout:Float):Void {
		var timeoutMs:Int = Std.int(timeout * 1000);
		_socket.ReceiveTimeout = timeoutMs;
		_socket.SendTimeout = timeoutMs;
	}

	public function waitForRead():Void {
		var timeout:Int = cs.Syntax.code("{0}.ReceiveTimeout", _socket);
		var end = Date.now().getTime() + ((timeout <= 0) ? Math.POSITIVE_INFINITY : timeout);
		var available:Int = cs.Syntax.code("{0}.Available", _socket);
		while (available == 0 && Date.now().getTime() < end) {
			cs.Syntax.code("global::System.Threading.Thread.Sleep(5)");
			available = cs.Syntax.code("{0}.Available", _socket);
		}
	}

	public function setBlocking(b:Bool):Void {
		_socket.Blocking = b;
	}

	public function setFastSend(b:Bool):Void {
		_socket.NoDelay = b;
	}

	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var map:Map<Int, Socket> = new Map();

		// Build handle-to-socket mapping
		if (read != null)
			for (s in read) {
				var handle:Int = cs.Syntax.code("{0}.Handle.ToInt32()", s._socket);
				map[handle] = s;
			}
		if (write != null)
			for (s in write) {
				var handle:Int = cs.Syntax.code("{0}.Handle.ToInt32()", s._socket);
				map[handle] = s;
			}
		if (others != null)
			for (s in others) {
				var handle:Int = cs.Syntax.code("{0}.Handle.ToInt32()", s._socket);
				map[handle] = s;
			}

		// Create ArrayLists with native sockets
		var rawRead:Dynamic = cs.Syntax.code("new global::System.Collections.ArrayList()");
		var rawWrite:Dynamic = cs.Syntax.code("new global::System.Collections.ArrayList()");
		var rawOthers:Dynamic = cs.Syntax.code("new global::System.Collections.ArrayList()");

		if (read != null)
			for (s in read)
				cs.Syntax.code("((global::System.Collections.ArrayList){0}).Add({1})", rawRead, s._socket);
		if (write != null)
			for (s in write)
				cs.Syntax.code("((global::System.Collections.ArrayList){0}).Add({1})", rawWrite, s._socket);
		if (others != null)
			for (s in others)
				cs.Syntax.code("((global::System.Collections.ArrayList){0}).Add({1})", rawOthers, s._socket);

		var microsec = timeout == null ? -1 : Std.int(timeout * 1000000);

		// Call native Socket.Select
		cs.Syntax.code("global::System.Net.Sockets.Socket.Select((global::System.Collections.IList){0}, (global::System.Collections.IList){1}, (global::System.Collections.IList){2}, {3})", rawRead, rawWrite, rawOthers, microsec);

		// Convert results back to Socket arrays
		inline function getOriginal(resultList:Dynamic):Array<Socket> {
			var a:Array<Socket> = [];
			var count:Int = cs.Syntax.code("((global::System.Collections.ArrayList){0}).Count", resultList);
			for (i in 0...count) {
				// ArrayList returns object, must cast to Socket before accessing Handle
				var handle:Int = cs.Syntax.code("((global::System.Net.Sockets.Socket)((global::System.Collections.ArrayList){0})[{1}]).Handle.ToInt32()", resultList, i);
				if (map.exists(handle))
					a.push(map[handle]);
			}
			return a;
		}

		return {
			read: getOriginal(rawRead),
			write: getOriginal(rawWrite),
			others: getOriginal(rawOthers),
		};
	}
}
