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

package js.node.net;

import haxe.extern.EitherType;
import js.node.events.EventEmitter;
import js.node.net.Socket.SocketAdress;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by the `Server` objects
**/
enum abstract ServerEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when the server has been bound after calling `Server.listen`.
	**/
	var Listening:ServerEvent<Void->Void> = "listening";

	/**
		Emitted when a new connection is made.
	**/
	var Connection:ServerEvent<Socket->Void> = "connection";

	/**
		Emitted when the server closes.
		Note that if connections exist, this event is not emitted until all connections are ended.
	**/
	var Close:ServerEvent<Void->Void> = "close";

	/**
		Emitted when an error occurs.
		The 'close' event will be called directly following this event. See example in discussion of server.listen.
	**/
	var Error:ServerEvent<Error->Void> = "error";
}

private typedef ServerListenOptionsBase = {
	@:optional var exclusive:Bool;
}

/**
	Options for the `Server.listen` method (TCP version).
**/
typedef ServerListenOptionsTcp = {
	> ServerListenOptionsBase,
	@:optional var port:Int;
	@:optional var host:String;
	@:optional var backlog:Int;
}

/**
	Options for the `Server.listen` method (UNIX version).
**/
typedef ServerListenOptionsUnix = {
	> ServerListenOptionsBase,
	@:optional var path:String;
}

/**
	This class is used to create a TCP or local server.
**/
@:jsRequire("net", "Server")
extern class Server extends EventEmitter<Server> {
	/**
		Begin accepting connections on the specified `port` and `hostname`.

		If the `hostname` is omitted, the server will accept connections on any IPv6 address (::) when IPv6 is available,
		or any IPv4 address (0.0.0.0) otherwise.
		A `port` value of zero will assign a random port.

		`backlog` is the maximum length of the queue of pending connections. The actual length will be determined
		by your OS through sysctl settings such as tcp_max_syn_backlog and somaxconn on linux.
		The default value of this parameter is 511 (not 512).

		When `path` is provided, start a local socket server listening for connections on the given path.

		When `handle` is provided, it should be either a server or socket (anything with an underlying `_handle` member),
		or a {fd: <n>} object. This will cause the server to accept connections on the specified handle,
		but it is presumed that the file descriptor or handle has already been bound to a port or domain socket.
		Listening on a file descriptor is not supported on Windows.

		This function is asynchronous. When the server has been bound, 'listening' event will be emitted.
		The last parameter `callback` will be added as an listener for the 'listening' event.
	**/
	@:overload(function(path:String, ?callback:Void->Void):Void {})
	@:overload(function(handle:EitherType<Dynamic, {fd:Int}>, ?callback:Void->Void):Void {})
	@:overload(function(port:Int, ?callback:Void->Void):Void {})
	@:overload(function(port:Int, backlog:Int, ?callback:Void->Void):Void {})
	@:overload(function(port:Int, hostname:String, ?callback:Void->Void):Void {})
	@:overload(function(port:Int, hostname:String, backlog:Int, ?callback:Void->Void):Void {})
	function listen(options:EitherType<ServerListenOptionsTcp, ServerListenOptionsUnix>, ?callback:Void->Void):Void;

	/**
		Stops the server from accepting new connections and keeps existing connections.
		This function is asynchronous, the server is finally closed when all connections are ended
		and the server emits a 'close' event.

		The optional callback will be called once the 'close' event occurs. Unlike that event,
		it will be called with an Error as its only argument if the server was not open when it was closed.
	**/
	@:overload(function(callback:Error->Void):Void {})
	function close(?callback:Void->Void):Void;

	/**
		Returns the bound address, the address family name and port of the server as reported by the operating system.
		Useful to find which port was assigned when giving getting an OS-assigned address.
	**/
	function address():SocketAdress;

	/**
		Calling `unref` on a server will allow the program to exit if this is the only active server in the event system.
		If the server is already `unref`d calling `unref` again will have no effect.
	**/
	function unref():Void;

	/**
		Opposite of `unref`, calling `ref` on a previously `unref`d server
		will not let the program exit if it's the only server left (the default behavior).

		If the server is `ref`d calling `ref` again will have no effect.
	**/
	function ref():Void;

	/**
		A boolean indicating whether or not the server is listening for connections.
	**/
	var listening(default, null):Bool;

	/**
		Set this property to reject connections when the server's connection count gets high.
		It is not recommended to use this option once a socket has been sent to a child with child_process.fork().
	**/
	var maxConnections:Int;

	/**
		The number of concurrent connections on the server.

		This becomes null when sending a socket to a child with child_process.fork().
		To poll forks and get current number of active connections use asynchronous `getConnections` instead.
	**/
	@:deprecated("please use `getConnections` instead")
	var connections(default, null):Null<Int>;

	/**
		Asynchronously get the number of concurrent connections on the server.
		Works when sockets were sent to forks.
	**/
	function getConnections(callback:Error->Int->Void):Void;
}
