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

import js.node.Buffer;
import js.node.events.EventEmitter.Event;
import js.node.net.Socket;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by `http.Server` class in addition to
	its parent `net.Server` class.
**/
enum abstract ServerEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted each time a request with an HTTP Expect: `100-continue` is received.
		If this event is not listened for, the server will automatically respond with a `100 Continue` as appropriate.

		Handling this event involves calling `response.writeContinue` if the client should continue
		to send the request body, or generating an appropriate HTTP response (e.g. 400 Bad Request) if the client
		should not continue to send the request body.

		When this event is emitted and handled, the 'request' event will not be emitted.
	**/
	#if haxe4
	var CheckContinue:ServerEvent<(request:IncomingMessage, response:ServerResponse) -> Void> = "checkContinue";
	#else
	var CheckContinue:ServerEvent<IncomingMessage->ServerResponse->Void> = "checkContinue";
	#end

	/**
		Emitted each time a request with an HTTP `Expect` header is received, where the value is not `100-continue`.
		If this event is not listened for, the server will automatically respond with a `417 Expectation Failed` as appropriate.

		When this event is emitted and handled, the `'request'` event will not be emitted.
	**/
	#if haxe4
	var CheckExpectation:ServerEvent<(request:IncomingMessage, response:ServerResponse) -> Void> = "checkExpectation";
	#else
	var CheckExpectation:ServerEvent<IncomingMessage->ServerResponse->Void> = "checkExpectation";
	#end

	/**
		If a client connection emits an `'error'` event, it will be forwarded here.
		Listener of this event is responsible for closing/destroying the underlying socket.
		For example, one may wish to more gracefully close the socket with a custom HTTP response instead of abruptly severing the connection.

		Default behavior is to try close the socket with a HTTP '400 Bad Request', or a HTTP '431 Request Header Fields Too Large'
		in the case of a `HPE_HEADER_OVERFLOW` error. If the socket is not writable it is immediately destroyed.
	**/
	#if haxe4
	var ClientError:ServerEvent<(exception:Error, socket:Socket) -> Void> = "clientError";
	#else
	var ClientError:ServerEvent<Error->Socket->Void> = "clientError";
	#end

	/**
		Emitted when the server closes.
	**/
	var Close:ServerEvent<Void->Void> = "close";

	/**
		Emitted each time a client requests an HTTP `CONNECT` method.
		If this event is not listened for, then clients requesting a `CONNECT` method will have their connections closed.

		After this event is emitted, the request's socket will not have a `'data'` event listener,
		meaning it will need to be bound in order to handle data sent to the server on that socket.
	**/
	#if haxe4
	var Connect:ServerEvent<(request:IncomingMessage, socekt:Socket, head:Buffer) -> Void> = "connect";
	#else
	var Connect:ServerEvent<IncomingMessage->Socket->Buffer->Void> = "connect";
	#end

	/**
		This event is emitted when a new TCP stream is established.
		`socket` is typically an object of type net.Socket. Usually users will not want to access this event.
		In particular, the socket will not emit `'readable'` events because of how the protocol parser attaches to the socket.
		The `socket` can also be accessed at `request.connection`.

		This event can also be explicitly emitted by users to inject connections into the HTTP server. In that case,
		any `Duplex` stream can be passed.

		If `socket.setTimeout()` is called here, the timeout will be replaced with `server.keepAliveTimeout`
		when the socket has served a request (if `server.keepAliveTimeout` is non-zero).
	**/
	var Connection:ServerEvent<Socket->Void> = "connection";

	/**
		Emitted each time there is a request.
		There may be multiple requests per connection (in the case of HTTP Keep-Alive connections).
	**/
	#if haxe4
	var Request:ServerEvent<(request:IncomingMessage, response:ServerResponse) -> Void> = "request";
	#else
	var Request:ServerEvent<IncomingMessage->ServerResponse->Void> = "request";
	#end

	/**
		Emitted each time a client requests an HTTP upgrade.
		Listening to this event is optional and clients cannot insist on a protocol change.

		After this event is emitted, the request's socket will not have a `'data'` event listener,
		meaning it will need to be bound in order to handle data sent to the server on that socket.
	**/
	#if haxe4
	var Upgrade:ServerEvent<(request:IncomingMessage, socket:Socket, buffer:Buffer) -> Void> = "upgrade";
	#else
	var Upgrade:ServerEvent<IncomingMessage->Socket->Buffer->Void> = "upgrade";
	#end
}

/**
	This class inherits `from net.Server`.
**/
@:jsRequire("http", "Server")
extern class Server extends js.node.net.Server {
	/**
		Limit the amount of time the parser will wait to receive the complete HTTP headers.

		In case of inactivity, the rules defined in `server.timeout` apply.
		However, that inactivity based timeout would still allow the connection to be kept open
		if the headers are being sent very slowly (by default, up to a byte per 2 minutes).
		In order to prevent this, whenever header data arrives an additional check is made that
		more than `server.headersTimeout` milliseconds has not passed since the connection was established.
		If the check fails, a `'timeout'` event is emitted on the server object, and (by default) the socket is destroyed.
		See [server.timeout](https://nodejs.org/api/http.html#http_server_timeout) for more information on how timeout behavior can be customized.

		Default: `40000`
	**/
	var headersTimeout:Int;

	/**
		Limits maximum incoming headers count. If set to 0, no limit will be applied.

		Default: `2000`
	**/
	var maxHeadersCount:Null<Int>;

	/**
		Sets the timeout value for sockets, and emits a `'timeout'` event on the Server object,
		passing the socket as an argument, if a timeout occurs.

		If there is a `'timeout'` event listener on the Server object, then it will be called with the timed-out socket as an argument.

		By default, the Server's timeout value is 2 minutes, and sockets are destroyed automatically if they time out.
		However, if a callback is assigned to the Server's `'timeout'` event, timeouts must be handled explicitly.

		To change the default timeout use the `--http-server-default-timeout` flag.
	**/
	function setTimeout(msecs:Int, ?callback:js.node.net.Socket->Void):Void;

	/**
		The number of milliseconds of inactivity before a socket is presumed to have timed out.

		A value of `0` will disable the timeout behavior on incoming connections.

		The socket timeout logic is set up on connection, so changing this value only affects new connections to the server,
		not any existing connections.

		To change the default timeout use the `--http-server-default-timeout` flag.

		Default: `120000` (2 minutes)
	**/
	var timeout:Int;

	/**
		The number of milliseconds of inactivity a server needs to wait for additional incoming data,
		after it has finished writing the last response, before a socket will be destroyed.
		If the server receives new data before the keep-alive timeout has fired, it will reset the regular inactivity timeout, i.e., `server.timeout`.

		A value of `0` will disable the keep-alive timeout behavior on incoming connections
		A value of 0 makes the http server behave similarly to Node.js versions prior to 8.0.0, which did not have a keep-alive timeout.

		The socket timeout logic is set up on connection, so changing this value only affects new connections to the server, not any existing connections.

		Default: `5000` (5 seconds).
	**/
	var keepAliveTimeout:Int;
}
