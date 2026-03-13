/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

import haxe.extern.EitherType;
import js.node.net.Server;
import js.node.net.Socket;

typedef NetCreateServerOptions = {
	> SocketOptionsBase,

	/**
		If true, then the socket associated with each incoming connection will be paused,
		and no data will be read from its handle.

		This allows connections to be passed between processes without any data being read by the original process.
		To begin reading data from a paused socket, call `resume`.

		Default: false
	**/
	@:optional var pauseOnConnect:Bool;
}

/**
	Options for the `Net.connect` method (TCP version).
**/
typedef NetConnectOptionsTcp = {
	> SocketOptions,
	> SocketConnectOptionsTcp,
}

/**
	Options for the `Net.connect` method (Local domain socket version).
**/
typedef NetConnectOptionsUnix = {
	> SocketOptions,
	> SocketConnectOptionsUnix,
}

/**
	Enumeration of possible values for `Net.isIP` return.
**/
enum abstract NetIsIPResult(Int) to Int {
	var Invalid = 0;
	var IPv4 = 4;
	var IPv6 = 6;
}

/**
	The net module provides you with an asynchronous network wrapper.
	It contains methods for creating both servers and clients (called streams).
**/
@:jsRequire("net")
extern class Net {
	/**
		Creates a new TCP server.

		The `connectionListener` argument is automatically set as a listener for the 'connection' event.
	**/
	@:overload(function(options:NetCreateServerOptions, ?connectionListener:Socket->Void):Server {})
	static function createServer(?connectionListener:Socket->Void):Server;

	/**
		A factory function, which returns a new `Socket` and automatically connects with the supplied `options`.

		The `options` are passed to both the `Socket` constructor and the `socket.connect` method.

		The `connectListener` parameter will be added as a listener for the `connect` event once.

		If `port` is provided, creates a TCP connection to `port` on `host`.
		If `host` is omitted, 'localhost' will be assumed.

		If `path` is provided, creates unix socket connection to `path`.

		Otherwise `options` argument should be provided.
	**/
	@:overload(function(path:String, ?connectListener:Void->Void):Socket {})
	@:overload(function(port:Int, ?connectListener:Void->Void):Socket {})
	@:overload(function(port:Int, host:String, ?connectListener:Void->Void):Socket {})
	static function connect(options:EitherType<NetConnectOptionsTcp, NetConnectOptionsUnix>, ?connectListener:Void->Void):Socket;

	/**
		Same as `connect`.
	**/
	@:overload(function(path:String, ?connectListener:Void->Void):Socket {})
	@:overload(function(port:Int, ?connectListener:Void->Void):Socket {})
	@:overload(function(port:Int, host:String, ?connectListener:Void->Void):Socket {})
	static function createConnection(options:EitherType<NetConnectOptionsTcp, NetConnectOptionsUnix>, ?connectListener:Void->Void):Socket;

	/**
		Tests if input is an IP address.
		Returns 0 for invalid strings, returns 4 for IP version 4 addresses, and returns 6 for IP version 6 addresses.
	**/
	static function isIP(input:String):NetIsIPResult;

	/**
		Returns true if input is a version 4 IP address, otherwise returns false.
	**/
	static function isIPv4(input:String):Bool;

	/**
		Returns true if input is a version 6 IP address, otherwise returns false.
	**/
	static function isIPv6(input:String):Bool;
}
