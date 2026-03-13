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
import js.node.events.EventEmitter.Event;
import js.node.net.Socket;
import js.node.stream.Writable;

/**
	Enumeration of events emitted by the `ServerResponse` objects in addition to its parent class events.
**/
enum abstract ServerResponseEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Indicates that the underlying connection was terminated.
	**/
	var Close:ServerResponseEvent<Void->Void> = "close";

	/**
		Emitted when the response has been sent.
		More specifically, this event is emitted when the last segment of the response header
		and body have been handed off to the operating system for transmission over the network.
		It does not imply that the client has received anything yet.
	**/
	var Finish:ServerResponseEvent<Void->Void> = "finish";
}

/**
	This object is created internally by an HTTP server — not by the user.
	It is passed as the second parameter to the 'request' event.
**/
@:jsRequire("http", "ServerResponse")
extern class ServerResponse extends Writable<ServerResponse> {
	/**
		This method adds HTTP trailing headers (a header but at the end of the message) to the response.

		Trailers will only be emitted if chunked encoding is used for the response;
		if it is not (e.g., if the request was HTTP/1.0), they will be silently discarded.

		Note that HTTP requires the 'Trailer' header to be sent if you intend to emit trailers,
		with a list of the header fields in its value.
	**/
	@:overload(function(headers:Array<Array<String>>):Void {})
	function addTrailers(headers:DynamicAccess<String>):Void;

	/**
		See `socket`.
	**/
	var connection(default, null):Socket;

	/**
		The `finished` property will be true if `end()` has been called.
	**/
	var finished(default, null):Bool;

	/**
		Flushes the response headers.
		See also: [request.flushHeaders()](https://nodejs.org/api/http.html#http_request_flushheaders).
	**/
	function flushHeaders():Void;

	/**
		Reads out a header that's already been queued but not sent to the client.
		The name is case-insensitive. The type of the return value depends on the arguments provided to `setHeader()`.
	**/
	function getHeader(name:String):haxe.extern.EitherType<String, Array<String>>;

	/**
		Returns an array containing the unique names of the current outgoing headers. All header names are lowercase.
	**/
	function getHeaderNames():Array<String>;

	/**
		Returns a shallow copy of the current outgoing headers. Since a shallow copy is used,
		array values may be mutated without additional calls to various header-related http module methods.
		The keys of the returned object are the header names and the values are the respective header values. All header names are lowercase.

		The object returned by the `getHeaders()` method does not prototypically inherit from the JavaScript Object.
		This means that typical `Object` methods such as `obj.toString()`, `obj.hasOwnProperty()`, and others are not defined and will not work.
	 */
	function getHeaders():DynamicAccess<haxe.extern.EitherType<String, Array<String>>>;

	/**
		Returns true if the header identified by `name` is currently set in the outgoing headers.
		The header name matching is case-insensitive.
	**/
	function hasHeader(name:String):Bool;

	/**
		Boolean (read-only). True if headers were sent, false otherwise.
	**/
	var headersSent(default, null):Bool;

	/**
		Removes a header that's queued for implicit sending.
	**/
	function removeHeader(name:String):Void;

	/**
		When true, the Date header will be automatically generated and sent in the response if it is not already present in the headers.
		Defaults to true.

		This should only be disabled for testing; HTTP requires the Date header in responses.
	**/
	var sendDate:Bool;

	/**
		Sets a single header value for implicit headers.
		If this header already exists in the to-be-sent headers, its value will be replaced.
		Use an array of strings here to send multiple headers with the same name.
		Non-string values will be stored without modification.
		Therefore, `getHeader()` may return non-string values.
		However, the non-string values will be converted to strings for network transmission.
	**/
	@:overload(function(name:String, value:Array<String>):Void {})
	function setHeader(name:String, value:String):Void;

	/**
		Sets the Socket's timeout value to `msecs`.
		If a callback is provided, then it is added as a listener on the `'timeout'` event on the response object.

		If no `'timeout'` listener is added to the request, the response, or the server, then sockets are destroyed when they time out.
		If a handler is assigned to the request, the response, or the server's `'timeout'` events, timed out sockets must be handled explicitly.
	**/
	function setTimeout(msecs:Int, ?callback:Void->Void):Void;

	/**
		Reference to the underlying socket. Usually users will not want to access this property.
		In particular, the socket will not emit `'readable'` events because of how the protocol parser attaches to the socket.
		After `end()`, the property is nulled. The `socket` may also be accessed via `connection`.
	**/
	var socket(default, null):Socket;

	/**
		When using implicit headers (not calling `writeHead` explicitly), this property controls the status code
		that will be sent to the client when the headers get flushed.

		After response header was sent to the client, this property indicates the status code which was sent out.
	**/
	var statusCode:Int;

	/**
		When using implicit headers (not calling `writeHead()` explicitly),
		this property controls the status message that will be sent to the client when the headers get flushed.
		If this is left as `undefined` then the standard message for the status code will be used.

		After response header was sent to the client, this property indicates the status message which was sent out.
	**/
	var statusMessage:String;

	/**
		Sends a HTTP/1.1 100 Continue message to the client, indicating that the request body should be sent.
		See the `'checkContinue'` event on `Server`.
	 */
	function writeContinue():Void;

	/**
		Sends a response header to the request.
		The status code is a 3-digit HTTP status code, like `404`. The last argument, `headers`, are the response headers.
		Optionally one can give a human-readable `statusMessage` as the second argument.

		This method must only be called once on a message and it must be called before `end()` is called.

		If `write()` or `end()` are called before calling this, the implicit/mutable headers will be calculated and call this function.

		When headers have been set with `setHeader()`, they will be merged with any headers passed to `writeHead()`, with the headers passed to `writeHead()` given precedence.

		If this method is called and `setHeader()` has not been called, it will directly write the supplied header values onto the network channel without caching internally,
		and the `getHeader()` on the header will not yield the expected result.
		If progressive population of headers is desired with potential future retrieval and modification, use `setHeader()` instead.

		`Content-Length` is given in bytes not characters.
		The above example works because the string `'hello world'` contains only single byte characters.
		If the body contains higher coded characters then `Buffer.byteLength()` should be used to determine the number of bytes in a given encoding.
		And Node.js does not check whether `Content-Length` and the length of the body which has been transmitted are equal or not.

		Attempting to set a header field name or value that contains invalid characters will result in a `TypeError` being thrown.
	**/
	@:overload(function(statusCode:Int, ?headers:DynamicAccess<String>):Void {})
	function writeHead(statusCode:Int, reasonPhrase:String, ?headers:DynamicAccess<String>):Void;

	/**
		Sends a HTTP/1.1 102 Processing message to the client, indicating that the request body should be sent.
	**/
	function writeProcessing():Void;

	// This field is defined in super class.
	// var writableEnded(default, null):Bool;
	// var writableFinished(default, null):Bool;
}
