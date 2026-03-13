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

package js.node.http;

import haxe.DynamicAccess;
import js.node.net.Socket;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	An `Agent` is responsible for managing connection persistence and reuse for HTTP clients.
	It maintains a queue of pending requests for a given host and port, reusing a single socket connection for each until the queue is empty,
	at which time the socket is either destroyed or put into a pool where it is kept to be used again for requests to the same host and port.
	Whether it is destroyed or pooled depends on the `keepAlive` option.

	Pooled connections have TCP Keep-Alive enabled for them, but servers may still close idle connections, in which case they will be removed
	from the pool and a new connection will be made when a new HTTP request is made for that host and port.
	Servers may also refuse to allow multiple requests over the same connection, in which case the connection will have to be remade for every
	request and cannot be pooled.
	The `Agent` will still make the requests to that server, but each one will occur over a new connection.

	When a connection is closed by the client or the server, it is removed from the pool.
	Any unused sockets in the pool will be unrefed so as not to keep the Node.js process running when there are no outstanding requests.
	(see [socket.unref()](https://nodejs.org/api/net.html#net_socket_unref)).

	It is good practice, to `destroy()` an Agent instance when it is no longer in use, because unused sockets consume OS resources.

	Sockets are removed from an agent when the socket emits either a `'close'` event or an `'agentRemove'` event.
	When intending to keep one HTTP request open for a long time without keeping it in the agent, something like the following may be done.

	An agent may also be used for an individual request. By providing `{agent: false}` as an option to the `http.get()` or `http.request()` functions,
	a one-time use `Agent` with default options will be used for the client connection.
**/
@:jsRequire("http", "Agent")
extern class Agent {
	/**
		`options` in socket.connect() are also supported.

		The default `http.globalAgent` that is used by `http.request()` has all of these values set to their respective defaults.

		To configure any of them, a custom `http.Agent` instance must be created.
	**/
	function new(?options:HttpAgentOptions);

	/**
		Produces a socket/stream to be used for HTTP requests.

		By default, this function is the same as `net.createConnection()`.
		However, custom agents may override this method in case greater flexibility is desired.

		A socket/stream can be supplied in one of two ways: by returning the socket/stream from this function,
		or by passing the socket/stream to `callback`.

		`callback` has a signature of `(err, stream)`.
	**/
	#if haxe4
	function createConnection(options:SocketConnectOptionsTcp, ?callback:(err:Error, stream:Socket) -> Void):Socket;
	#else
	function createConnection(options:SocketConnectOptionsTcp, ?callback:Error->Socket->Void):Socket;
	#end

	/**
		Called when `socket` is detached from a request and could be persisted by the `Agent`.

		This method can be overridden by a particular `Agent` subclass.
		If this method returns a falsy value, the socket will be destroyed instead of persisting it for use with the next request.
	**/
	function keepSocketAlive(socket:Socket):Void;

	/**
		Called when `socket` is attached to `request` after being persisted because of the keep-alive options.

		This method can be overridden by a particular `Agent` subclass.
	**/
	function reuseSocket(socket:Socket, request:ClientRequest):Void;

	/**
		Destroy any sockets that are currently in use by the agent.

		It is usually not necessary to do this. However, if using an agent with `keepAlive` enabled,
		then it is best to explicitly shut down the agent when it will no longer be used. Otherwise,
		sockets may hang open for quite a long time before the server terminates them.
	**/
	function destroy():Void;

	/**
		An object which contains arrays of sockets currently awaiting use by the agent when keepAlive is enabled.
		Do not modify.
	 */
	var freeSockets(default, null):DynamicAccess<Array<Socket>>;

	/**
		Get a unique name for a set of request options, to determine whether a connection can be reused.
		For an HTTP agent, this returns `host:port:localAddress` or `host:port:localAddress:family`.
		For an HTTPS agent, the name includes the CA, cert, ciphers, and other HTTPS/TLS-specific options that determine socket reusability.
	**/
	function getName(options:js.node.Http.HttpRequestOptions):String;

	/**
		By default set to `256`.
		For agents with `keepAlive` enabled, this sets the maximum number of sockets that will be left open in the free state.
	**/
	var maxFreeSockets:Float;

	/**
		By default set to `Infinity`.
		Determines how many concurrent sockets the agent can have open per origin. Origin is the returned value of `getName()`.
	**/
	var maxSockets:Float;

	/**
		An object which contains queues of requests that have not yet been assigned to sockets.
		Do not modify.
	**/
	var requests(default, null):DynamicAccess<Array<ClientRequest>>;

	/**
		An object which contains arrays of sockets currently in use by the agent.
		Do not modify.
	**/
	var sockets(default, null):DynamicAccess<Array<Socket>>;
}

/**
	Options for `Agent` constructor.
**/
typedef HttpAgentOptions = {
	/**
		Keep sockets around even when there are no outstanding requests, so they can be used for future requests
		without having to reestablish a TCP connection.
		Not to be confused with the `keep-alive` value of the `Connection` header.
		The `Connection: keep-alive` header is always sent when using an agent except when the `Connection` header
		is explicitly specified or when the `keepAlive` and `maxSockets` options are respectively set to `false` and `Infinity`,
		in which case `Connection: close` will be used.

		Default: `false`
	**/
	@:optional var keepAlive:Bool;

	/**
		When using the `keepAlive` option, specifies the [initial delay](https://nodejs.org/api/net.html#net_socket_setkeepalive_enable_initialdelay) for TCP Keep-Alive packets.
		Ignored when the `keepAlive` option is `false` or `undefined`.

		Default: `1000`.
	**/
	@:optional var keepAliveMsecs:Int;

	/**
		Maximum number of sockets to allow per host. Each request will use a new socket until the maximum is reached.

		Default: `Infinity`.
	**/
	@:optional var maxSockets:Int;

	/**
		Maximum number of sockets to leave open in a free state. Only relevant if `keepAlive` is set to `true`.

		Default: `256`.
	**/
	@:optional var maxFreeSockets:Int;

	/**
		Socket timeout in milliseconds. This will set the timeout when the socket is created.
	**/
	@:optional var timeout:Int;
}
