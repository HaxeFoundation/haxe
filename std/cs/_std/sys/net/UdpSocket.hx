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

import haxe.extern.Rest;
import sys.net.Socket;
import cs.NativeArray;
import cs.system.collections.ArrayList;
import cs.system.net.IPEndPoint;
import cs.system.net.EndPoint;
import cs.system.net.IPAddress;
import cs.system.net.sockets.AddressFamily;
import cs.system.net.sockets.NetworkStream;
import cs.system.net.sockets.ProtocolType;
import cs.system.net.sockets.SocketFlags;
import cs.system.net.sockets.SocketShutdown;
import cs.system.net.sockets.SocketType;
import cs.system.threading.Thread;
import cs.system.net.sockets.Socket in NativeSocket;
import cs.types.UInt8;
import cs.Ref;
import haxe.io.Bytes;
import haxe.io.Error;
import haxe.io.Input;
import haxe.io.Output;

@:coreapi
class UdpSocket extends Socket {
	public function new() {
		super();
	}

	override private function init():Void {
		sock = new NativeSocket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
	}

	override public function bind(host:Host, port:Int):Void {
		sock = new NativeSocket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
		var endpoint:IPEndPoint = new IPEndPoint(host.ipAddress, port);
		sock.Bind(endpoint);
	}

	public function sendTo(buf:haxe.io.Bytes, pos:Int, len:Int, addr:Address):Int {
		var data = new NativeArray<UInt8>(len);
		var indices:NativeArray<Int>;
		for (i in 0...len) {
			indices = NativeArray.make(i);
			data.SetValue(cast buf.get(pos + i), indices);
		}
		var host = addr.getHost();
		var ip:IPAddress = IPAddress.Parse(host.toString());
		var endpoint:IPEndPoint = new IPEndPoint(ip, addr.port);
		return this.sock.SendTo(data, endpoint);
	}

	public function readFrom(buf:haxe.io.Bytes, pos:Int, len:Int, addr:Address):Int {
		var endpoint:EndPoint = cast new IPEndPoint(IPAddress.Any, 0);
		var data:NativeArray<UInt8> = new NativeArray(len);
		var length:Int = -1;
		try {
			length = this.sock.ReceiveFrom(data, endpoint);
		} catch (e:Dynamic) {
			return length;
		}
		var ipEndpoint:IPEndPoint = cast endpoint;
		addr.host = ipEndpoint.Address.Address.high;
		addr.port = ipEndpoint.Port;
		var i:Int = 0;
		for (each in data.iterator()) {
			buf.set(pos + i, each);
			i += 1;
		}
		return length;
	}

	public function setBroadcast(b:Bool):Void {
		sock.EnableBroadcast = b;
	}
}
