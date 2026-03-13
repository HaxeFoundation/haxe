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
import js.node.stream.Readable;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by the `IncomingMessage` objects in addition to its parent class events.
**/
enum abstract IncomingMessageeEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when the request has been aborted.
	**/
	var Aborted:IncomingMessageeEvent<Void->Void> = "aborted";

	/**
		Indicates that the underlying connection was closed.
	**/
	var Close:IncomingMessageeEvent<Void->Void> = "close";
}

/**
	An `IncomingMessage` object is created by `http.Server` or `http.ClientRequest` and passed as the first argument to the `'request'` and `'response'` event respectively.
	It may be used to access response status, headers and data.

	It implements the `Readable Stream` interface, as well as the following additional events, methods, and properties.
**/
@:jsRequire("http", "IncomingMessage")
extern class IncomingMessage extends Readable<IncomingMessage> {
	/**
		The `aborted` property will be `true` if the request has been aborted.
	**/
	var aborted(default, null):Bool;

	/**
		The `complete` property will be `true` if a complete HTTP message has been received and successfully parsed.
	**/
	var complete(default, null):Bool;

	/**
		Calls `destroy()` on the socket that received the `IncomingMessage`.
		If `error` is provided, an `'error'` event is emitted and `error` is passed as an argument to any listeners on the event.
	**/
	override function destroy(?error:Error):IncomingMessage;

	/**
		The request/response headers object.

		Key-value pairs of header names and values. Header names are lower-cased.

		Duplicates in raw headers are handled in the following ways, depending on the header name:

		- Duplicates of `age`, `authorization`, `content-length`, `content-type`, `etag`, `expires`, `from`, `host`, `if-modified-since`, `if-unmodified-since`,
		  `last-modified`, `location`, `max-forwards`, `proxy-authorization`, `referer`, `retry-after`, or `user-agent` are discarded.
		- `set-cookie` is always an array. Duplicates are added to the array.
		- For duplicate `cookie` headers, the values are joined together with '; '.
		- For all other headers, the values are joined together with ', '.
	**/
	var headers(default, null):DynamicAccess<haxe.extern.EitherType<String, Array<String>>>;

	/**
		In case of server request, the HTTP version sent by the client.
		In the case of client response, the HTTP version of the connected-to server.
		Probably either `'1.1'` or `'1.0'`.
	**/
	var httpVersion(default, null):String;

	/**
		HTTP Version first integer
	**/
	var httpVersionMajor(default, null):Int;

	/**
		HTTP Version second integer
	**/
	var httpVersionMinor(default, null):Int;

	/**
		*Only valid for request obtained from* `Server`.

		The request method as a string.
		Read only. Example: `'GET'`, `'DELETE'`.
	**/
	var method(default, null):Method;

	/**
		The raw request/response headers list exactly as they were received.

		The keys and values are in the same list. It is not a list of tuples. So, the even-numbered offsets are key values,
		and the odd-numbered offsets are the associated values.

		Header names are not lowercased, and duplicates are not merged.
	**/
	var rawHeaders(default, null):Array<String>;

	/**
		The raw request/response trailer keys and values exactly as they were received.
		Only populated at the `'end'` event.
	**/
	var rawTrailers(default, null):Array<String>;

	/**
		Calls `connection.setTimeout(msecs, callback)`.
	**/
	function setTimeout(msecs:Int, ?callback:Void->Void):Void;

	/**
		The `Socket` object associated with the connection.

		With HTTPS support, use `request.socket.getPeerCertificate()` to obtain the client's authentication details.
	**/
	var socket(default, null):Socket;

	/**
		Alias for `socket`.
	**/
	var connection(default, null):Socket;

	/**
		*Only valid for response obtained from* `ClientRequest`.
		The 3-digit HTTP response status code. E.G. `404`.
	**/
	var statusCode(default, null):Int;

	/**
		*Only valid for response obtained from* `ClientRequest`.
		The HTTP response status message (reason phrase). E.G. `OK` or `Internal Server Error`.
	**/
	var statusMessage(default, null):String;

	/**
		The request/response trailers object.
		Only populated after the `'end'` event.
	**/
	var trailers(default, null):DynamicAccess<String>;

	/**
		*Only valid for request obtained from* `Server`.

		Request URL string. This contains only the URL that is present in the actual HTTP request.
	**/
	var url(default, null):String;
}
