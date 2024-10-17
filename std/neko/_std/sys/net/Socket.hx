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

import haxe.Int32;
import haxe.exceptions.NotImplementedException;
import haxe.io.Error;
import neko.NativeArray;

@:callable
@:coreType
abstract SocketHandle {}

private class SocketOutput extends haxe.io.Output {
	var __s:SocketHandle;

	public function new(s) {
		__s = s;
	}

	public override function writeByte(c:Int) {
		try {
			socket_send_char(__s, c);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw Blocked;
			else if (e == "EOF")
				throw new haxe.io.Eof();
			else
				throw Custom(e);
		}
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		return try {
			socket_send(__s, buf.getData(), pos, len);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw Blocked;
			else
				throw Custom(e);
		}
	}

	public override function close() {
		super.close();
		if (__s != null)
			socket_close(__s);
	}

	private static var socket_close = neko.Lib.load("std", "socket_close", 1);
	private static var socket_send_char = neko.Lib.load("std", "socket_send_char", 2);
	private static var socket_send = neko.Lib.load("std", "socket_send", 4);
}

private class SocketInput extends haxe.io.Input {
	var __s:SocketHandle;

	public function new(s) {
		__s = s;
	}

	public override function readByte():Int {
		return try {
			socket_recv_char(__s);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw Blocked;
			else if (__s == null)
				throw Custom(e);
			else
				throw new haxe.io.Eof();
		}
	}

	public override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		var r;
		try {
			r = socket_recv(__s, buf.getData(), pos, len);
		} catch (e:Dynamic) {
			if (e == "Blocking")
				throw Blocked;
			else
				throw Custom(e);
		}
		if (r == 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (__s != null)
			socket_close(__s);
	}

	private static var socket_recv = neko.Lib.load("std", "socket_recv", 4);
	private static var socket_recv_char = neko.Lib.load("std", "socket_recv_char", 1);
	private static var socket_close = neko.Lib.load("std", "socket_close", 1);
}

@:coreApi
@:keepInit
class Socket {
	private var __s:SocketHandle;

	public var input(default, null):haxe.io.Input;
	public var output(default, null):haxe.io.Output;
	public var custom:Dynamic;

	private static function __init__():Void {
		Socket.socket_new = cast neko.Lib.load("std", "socket_new", 1);
		Socket.socket_close = cast neko.Lib.load("std", "socket_close", 1);
		Socket.socket_write = cast neko.Lib.load("std", "socket_write", 2);
		Socket.socket_read = cast neko.Lib.load("std", "socket_read", 1);
		Socket.socket_connect = cast neko.Lib.load("std", "socket_connect", 3);
		Socket.socket_listen = cast neko.Lib.load("std", "socket_listen", 2);
		Socket.socket_select = cast neko.Lib.load("std", "socket_select", 4);
		Socket.socket_bind = cast neko.Lib.load("std", "socket_bind", 3);
		Socket.socket_accept = cast neko.Lib.load("std", "socket_accept", 1);
		Socket.socket_peer = cast neko.Lib.load("std", "socket_peer", 1);
		Socket.socket_host = cast neko.Lib.load("std", "socket_host", 1);
		Socket.socket_set_timeout = cast neko.Lib.load("std", "socket_set_timeout", 2);
		Socket.socket_shutdown = cast neko.Lib.load("std", "socket_shutdown", 3);
		Socket.socket_set_blocking = cast neko.Lib.load("std", "socket_set_blocking", 2);
		Socket.socket_set_fast_send = cast neko.Lib.loadLazy("std", "socket_set_fast_send", 2);
		Socket.int32_new = cast neko.Lib.load("std", "int32_new", 1);
		neko.Lib.load("std", "socket_init", 0)();
	}

	public function new():Void {
		init();
	}

	private function init():Void {
		if (this.__s == null) {
			this.__s = socket_new(false);
		}
		input = new SocketInput(__s);
		output = new SocketOutput(__s);
	}

	public function close():Void {
		socket_close(__s);
		untyped {
			input.__s = null;
			output.__s = null;
		}
		input.close();
		output.close();
	}

	public function read():String {
		return new String(socket_read(__s));
	}

	public function write(content:String):Void {
		socket_write(__s, untyped content.__s);
	}

	public function connect(host:Host, port:Int):Void {
		final address = @:privateAccess host.getAddressesSorted(PreferIPv4)[0];
		switch (address) {
			case V4(addr):
				final ipv4 = addr.asNetworkOrderInt();
				try {
					socket_connect(__s, int32_new(ipv4), port);
				} catch (s:String) {
					if (s == "std@socket_connect")
						throw 'Failed to connect on $addr:$port';
					else if (s == "Blocking") {
						// Do nothing, this is not a real error, it simply indicates
						// that a non-blocking connect is in progress
					} else {
						neko.Lib.rethrow(s);
					}
				}
			case V6(_):
				throw new UnsupportedFamilyException("Neko does not support connecting to IPv6 addresses");
		}
	}

	public function listen(connections:Int):Void {
		socket_listen(__s, connections);
	}

	public function shutdown(read:Bool, write:Bool):Void {
		socket_shutdown(__s, read, write);
	}

	public function bind(host:Host, port:Int):Void {
		final address = @:privateAccess host.getAddressesSorted(PreferIPv4)[0];
		switch (address) {
			case V4(addr):
				final ipv4:Int32 = addr.asNetworkOrderInt();
				socket_bind(this.__s, int32_new(ipv4), port);
			case V6(_):
				throw new UnsupportedFamilyException("Neko does not support binding to IPv6 interfaces");
		}
	}

	public function accept():Socket {
		var c = socket_accept(__s);
		var s = Type.createEmptyInstance(Socket);
		s.__s = c;
		s.input = new SocketInput(c);
		s.output = new SocketOutput(c);
		return s;
	}

	public function peer():{host:Host, port:Int} {
		var a:Dynamic = socket_peer(__s);
		if (a == null) {
			return null;
		}
		final ipv4 = Ipv4Address.fromNetworkOrderInt(cast a[0]);
		final host = new Host(ipv4.toString());
		final port:Int = cast a[1];
		return {host: host, port: port};
	}

	public function host():{host:Host, port:Int} {
		var a:Dynamic = socket_host(__s);
		if (a == null) {
			return null;
		}
		final ipv4 = Ipv4Address.fromNetworkOrderInt(cast a[0]);
		final host = new Host(ipv4.toString());
		final port:Int = cast a[1];
		return {host: host, port: port};
	}

	public function setTimeout(timeout:Float):Void {
		socket_set_timeout(__s, timeout);
	}

	public function waitForRead():Void {
		select([this], null, null, null);
	}

	public function setBlocking(b:Bool):Void {
		socket_set_blocking(__s, b);
	}

	public function setFastSend(b:Bool):Void {
		socket_set_fast_send(__s, b);
	}

	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var c = untyped __dollar__hnew(1);
		final f:(Array<Socket>) -> NativeArray<SocketHandle> = (a) -> {
			if (a == null) {
				return null;
			}

			final length = a.length;
			final r:NativeArray<SocketHandle> = NativeArray.alloc(length);
			var i = 0;
			while (i < length) untyped {
				r[i] = a[i].__s;
				__dollar__hadd(c, a[i].__s, a[i]);
				i += 1;
			}
			return r;
		}
		var neko_array = socket_select(f(read), f(write), f(others), timeout);

		final g = function(a):Array<Socket> {
			if (a == null) {
				return null;
			}

			var r = new Array();
			var i = 0;
			while (i < untyped __dollar__asize(a)) {
				var t = untyped __dollar__hget(c, a[i], null);
				if (t == null)
					throw "Socket object not found.";
				r[i] = t;
				i += 1;
			}
			return r;
		}

		return {
			read: g(neko_array[0]),
			write: g(neko_array[1]),
			others: g(neko_array[2])
		};
	}

	private static dynamic function socket_new(datagram:Bool):SocketHandle {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_close(handle:SocketHandle):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_write(handle:SocketHandle, stringData:Any):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_read(handle:SocketHandle):Dynamic {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_connect(handle:SocketHandle, ipv4:Int32, port:Int):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_listen(handle:SocketHandle, connections:Int):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_select(readHandlesArray:NativeArray<SocketHandle>, writeHandlesArray:NativeArray<SocketHandle>,
			otherHandlesArray:NativeArray<SocketHandle>, timeout:Float):NativeArray<Dynamic> {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_bind(handle:SocketHandle, ipv4:Int32, port:Int):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_accept(serverHandle:SocketHandle):SocketHandle {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_peer(handle:SocketHandle):Dynamic {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_host(handle:SocketHandle):Dynamic {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_set_timeout(handle:SocketHandle, timeout:Float):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_shutdown(handle:SocketHandle, read:Bool, write:Bool):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_set_blocking(handle:SocketHandle, blocking:Bool):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function socket_set_fast_send(handle:SocketHandle, fastSend:Bool):Void {
		throw new NotImplementedException("Neko function was not loaded");
	}

	private static dynamic function int32_new(v:Int32):Int32 {
		throw new NotImplementedException("Neko function was not loaded");
	}
}
