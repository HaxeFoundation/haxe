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
import js.node.Buffer;
import js.node.events.EventEmitter.Event;
import js.node.net.Socket;
import js.node.stream.Writable;

/**
	Enumeration of events emitted by `ClientRequest`
**/
enum abstract ClientRequestEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when the request has been aborted by the client.
		This event is only emitted on the first call to `abort()`.
	**/
	var Abort:ClientRequestEvent<Void->Void> = "abort";

	/**
		Emitted each time a server responds to a request with a `CONNECT` method.
		If this event is not being listened for, clients receiving a `CONNECT` method will have their connections closed.
	**/
	#if haxe4
	var Connect:ClientRequestEvent<(response:IncomingMessage, socket:Socket, head:Buffer) -> Void> = "connect";
	#else
	var Connect:ClientRequestEvent<IncomingMessage->Socket->Buffer->Void> = "connect";
	#end

	/**
		Emitted when the server sends a '100 Continue' HTTP response,
		usually because the request contained 'Expect: 100-continue'.
		This is an instruction that the client should send the request body.
	**/
	var Continue:ClientRequestEvent<Void->Void> = "continue";

	/**
		Emitted when the server sends a 1xx intermediate response (excluding 101 Upgrade).
		The listeners of this event will receive an object containing the HTTP version, status code, status message,
		key-value headers object, and array with the raw header names followed by their respective values.
	**/
	var Information:ClientRequestEvent<InformationEventData->Void> = "information";

	/**
		Emitted when a response is received to this request. This event is emitted only once.
	**/
	var Response:ClientRequestEvent<IncomingMessage->Void> = "response";

	/**
		Emitted after a socket is assigned to this request.
	**/
	var Socket:ClientRequestEvent<Socket->Void> = "socket";

	/**
		Emitted when the underlying socket times out from inactivity.
		This only notifies that the socket has been idle. The request must be aborted manually.

		See also: [request.setTimeout()](https://nodejs.org/api/http.html#http_request_settimeout_timeout_callback).
	**/
	var Timeout:ClientRequestEvent<Socket->Void> = "timeout";

	/**
		Emitted each time a server responds to a request with an upgrade.
		If this event is not being listened for and the response status code is 101 Switching Protocols,
		clients receiving an upgrade header will have their connections closed.
	**/
	#if haxe4
	var Upgrade:ClientRequestEvent<(response:IncomingMessage, socket:Socket, head:Buffer) -> Void> = "upgrade";
	#else
	var Upgrade:ClientRequestEvent<IncomingMessage->Socket->Buffer->Void> = "upgrade";
	#end
}

/**
	This object is created internally and returned from http.request().
	It represents an in-progress request whose header has already been queued.
	The header is still mutable using the `setHeader(name, value)`, `getHeader(name)`, `removeHeader(name)` API.
	The actual header will be sent along with the first data chunk or when calling `request.end()`.

	To get the response, add a listener for `'response'` to the request object.
	`'response'` will be emitted from the request object when the response headers have been received.
	The `'response'` event is executed with one argument which is an instance of `http.IncomingMessage`.

	During the `'response'` event, one can add listeners to the response object; particularly to listen for the `'data'` event.

	If no `'response'` handler is added, then the response will be entirely discarded. However,
	if a `'response'` event handler is added, then the data from the response object *must* be consumed,
	either by calling `response.read()` whenever there is a `'readable'` event, or by adding a `'data'` handler,
	or by calling the `.resume()` method. Until the data is consumed, the `'end'` event will not fire.
	Also, until the data is read it will consume memory that can eventually lead to a 'process out of memory' error.

	Unlike the `request` object, if the response closes prematurely, the response object does not emit an `'error'` event
	but instead emits the `'aborted'` event.

	Node.js does not check whether Content-Length and the length of the body which has been transmitted are equal or not.
**/
@:jsRequire("http", "ClientRequest")
extern class ClientRequest extends Writable<ClientRequest> {
	/**
		Marks the request as aborting. Calling this will cause remaining data in the response to be dropped and the socket to be destroyed.
	**/
	function abort():Void;

	/**
		The request.aborted property will be true if the request has been aborted.
	**/
	var aborted(default, null):Bool;

	/**
		See `request.socket`.
	**/
	var connection(default, null):Socket;

	/**
		The `response.finished` property will be true if `response.end()` has been called.
	**/
	var finished(default, null):Bool;

	/**
		Flush the request headers.

		For efficiency reasons, node.js normally buffers the request headers until you call `request.end()`
		or write the first chunk of request data. It then tries hard to pack the request headers and data
		into a single TCP packet.

		That's usually what you want (it saves a TCP round-trip) but not when the first data isn't sent
		until possibly much later. `flushHeaders` lets you bypass the optimization and kickstart the request.
	**/
	function flushHeaders():Void;

	/**
		Reads out a header on the request. The name is case-insensitive.
		The type of the return value depends on the arguments provided to `request.setHeader()`.
	**/
	function getHeader(name:String):haxe.extern.EitherType<String, Array<String>>;

	/**
		Limits maximum response headers count. If set to 0, no limit will be applied.

		Default: `2000`
	**/
	var maxHeadersCount:Null<Int>;

	/**
		The request path.
	**/
	var path(default, null):String;

	/**
		Removes a header that's already defined into headers object.
	**/
	function removeHeader(name:String):Void;

	/**
		Sets a single header value for headers object.
		If this header already exists in the to-be-sent headers, its value will be replaced.
		Use an array of strings here to send multiple headers with the same name.
		Non-string values will be stored without modification. Therefore, `request.getHeader()` may return non-string values.
		However, the non-string values will be converted to strings for network transmission.
	**/
	@:overload(function(name:String, value:Array<String>):Void {})
	function setHeader(name:String, value:String):Void;

	/**
		Once a socket is assigned to this request and is connected
		`socket.setNoDelay` will be called.
	**/
	function setNoDelay(?noDelay:Bool):Void;

	/**
		Once a socket is assigned to this request and is connected
		`socket.setKeepAlive`() will be called.
	**/
	@:overload(function(?initialDelay:Int):Void {})
	function setSocketKeepAlive(enable:Bool, ?initialDelay:Int):Void;

	/**
		Once a socket is assigned to this request and is connected `socket.setTimeout()` will be called.
	**/
	function setTimeout(timeout:Int, ?callback:Socket->Void):ClientRequest;

	/**
		Reference to the underlying socket. Usually users will not want to access this property.
		In particular, the socket will not emit `'readable'` events because of how the protocol parser attaches to the socket.
		The `socket` may also be accessed via `request.connection`.
	 */
	var socket(default, null):Socket;

	// This field is defined in super class.
	// var writableEnded(default, null):Bool;
	// var writableFinished(default, null):Bool;
}

typedef InformationEventData = {
	var httpVersion:String;
	var httpVersionMajor:Int;
	var httpVersionMinor:Int;
	var statusCode:Int;
	var statusMessage:String;
	var headers:DynamicAccess<String>;
	var rawHeaders:Array<String>;
}
