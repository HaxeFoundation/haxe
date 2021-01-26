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

import cs.NativeArray;
import cs.system.collections.ArrayList;
import cs.system.net.IPEndPoint;
import cs.system.net.sockets.AddressFamily;
import cs.system.net.sockets.NetworkStream;
import cs.system.net.sockets.ProtocolType;
import cs.system.net.sockets.SocketFlags;
import cs.system.net.sockets.SocketShutdown;
import cs.system.net.sockets.SocketType;
import cs.system.threading.Thread;
import cs.system.net.sockets.Socket in NativeSocket;
import cs.types.UInt8;
import haxe.io.Bytes;
import haxe.io.Error;
import haxe.io.Input;
import haxe.io.Output;

@:coreApi
class Socket {
	private var sock:NativeSocket = null;

	public var input(default, null):haxe.io.Input;

	public var output(default, null):haxe.io.Output;

	public var custom:Dynamic;

	/**
		Creates a new unconnected socket.
	**/
	public function new() : Void {
		init();
	}
	
	public function new():Void {
		sock = new NativeSocket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
		sock.Blocking = true;
	}

	public function close():Void {
		sock.Close();
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
		sock.Connect(host.ipAddress, port);
		if (sock.Connected) {
			this.output = new cs.io.NativeOutput(new NetworkStream(sock));
			this.input = new cs.io.NativeInput(new NetworkStream(sock));
		} else {
			throw "Connection failed.";
		}
	}

	public function listen(connections:Int):Void {
		sock.Listen(connections);
	}

	public function shutdown(read:Bool, write:Bool):Void {
		if (read && write) {
			sock.Shutdown(SocketShutdown.Both);
			input = null;
			output = null;
		} else if (read) {
			sock.Shutdown(SocketShutdown.Receive);
			input = null;
		} else if (write) {
			sock.Shutdown(SocketShutdown.Send);
			output = null;
		}
	}

	public function bind(host:Host, port:Int):Void {
		sock = new NativeSocket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
		sock.Bind(new IPEndPoint(host.ipAddress, port));
	}

	public function accept():Socket {
		var r = new Socket();
		r.sock = sock.Accept();
		r.output = new cs.io.NativeOutput(new NetworkStream(r.sock));
		r.input = new cs.io.NativeInput(new NetworkStream(r.sock));
		return r;
	}

	public function peer():{host:Host, port:Int} {
		var remoteIP = cast(sock.RemoteEndPoint, IPEndPoint);
		return {host: new Host(remoteIP.Address.ToString()), port: remoteIP.Port};
	}

	public function host():{host:Host, port:Int} {
		var localIP = cast(sock.LocalEndPoint, IPEndPoint);
		return {host: new Host(localIP.Address.ToString()), port: localIP.Port};
	}

	public function setTimeout(timeout:Float):Void {
		sock.ReceiveTimeout = sock.SendTimeout = Math.round(timeout * 1000);
	}

	public function waitForRead():Void {
		var end = Date.now().getTime() + ((sock.ReceiveTimeout <= 0) ? Math.POSITIVE_INFINITY : sock.ReceiveTimeout);
		while (sock.Available == 0 && Date.now().getTime() < end) {
			Thread.Sleep(5);
		}
	}

	public function setBlocking(b:Bool):Void {
		sock.Blocking = b;
	}

	public function setFastSend(b:Bool):Void {
		sock.NoDelay = b;
	}

	static public function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var map:Map<Int, Socket> = new Map();
		inline function addSockets(sockets:Array<Socket>) {
			if (sockets != null)
				for (s in sockets)
					map[s.sock.Handle.ToInt32()] = s;
		}
		inline function getRaw(sockets:Array<Socket>):ArrayList {
			var a = new ArrayList(sockets == null ? 0 : sockets.length);
			if (sockets != null)
				for (s in sockets) {
					a.Add(s.sock);
				}
			return a;
		}
		inline function getOriginal(result:ArrayList) {
			var a:Array<Socket> = [];
			for (i in 0...result.Count) {
				var s:NativeSocket = result[i];
				a.push(map[s.Handle.ToInt32()]);
			}
			return a;
		}

		addSockets(read);
		addSockets(write);
		addSockets(others);

		// unwrap Sockets into native sockets
		var rawRead:ArrayList = getRaw(read),
			rawWrite:ArrayList = getRaw(write),
			rawOthers:ArrayList = getRaw(others);
		var microsec = timeout == null ? -1 : Std.int(timeout * 1000000);
		NativeSocket.Select(rawRead, rawWrite, rawOthers, microsec);
		// convert native sockets back to Socket objects
		return {
			read: getOriginal(rawRead),
			write: getOriginal(rawWrite),
			others: getOriginal(rawOthers),
		}
	}
}
