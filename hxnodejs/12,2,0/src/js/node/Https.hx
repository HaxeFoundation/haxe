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

import js.node.http.ClientRequest;
import js.node.http.IncomingMessage;
import js.node.http.ServerResponse;
import js.node.https.*;
import js.node.url.URL;

/**
	HTTPS is the HTTP protocol over TLS/SSL.
	In Node.js this is implemented as a separate module.
**/
@:jsRequire("https")
extern class Https {
	/**
		Returns a new HTTPS web server object.
	**/
	#if haxe4
	@:overload(function(options:HttpsCreateServerOptions, ?requestListener:(request:IncomingMessage, response:ServerResponse) -> Void):Server {})
	static function createServer(?requestListener:(request:IncomingMessage, response:ServerResponse) -> Void):Server;
	#else
	@:overload(function(options:HttpsCreateServerOptions, ?requestListener:IncomingMessage->ServerResponse->Void):Server {})
	static function createServer(?requestListener:IncomingMessage->ServerResponse->Void):Server;
	#end

	/**
		Like `Http.get` but for HTTPS.

		`options` can be an object, a string, or a `URL` object.
		If `options` is a string, it is automatically parsed with `new URL()`.
		If it is a `URL` object, it will be automatically converted to an ordinary `options` object.
	**/
	@:overload(function(url:URL, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:URL, options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:String, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:String, options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest {})
	static function get(options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest;

	/**
		Global instance of `https.Agent` for all HTTPS client requests.
	**/
	static var globalAgent:Agent;

	/**
		Makes a request to a secure web server.

		The following additional `options` from `tls.connect()` are also accepted:
		`ca`, `cert`, `ciphers`, `clientCertEngine`, `crl`, `dhparam`, `ecdhCurve`, `honorCipherOrder`, `key`, `passphrase`, `pfx`,
		`rejectUnauthorized`, `secureOptions`, `secureProtocol`, `servername`, `sessionIdContext`.

		`options` can be an object, a string, or a `URL` object.
		If `options` is a string, it is automatically parsed with `new URL()`.
		If it is a `URL` object, it will be automatically converted to an ordinary `options` object.
	**/
	@:overload(function(url:URL, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:URL, options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:String, ?callback:IncomingMessage->Void):ClientRequest {})
	@:overload(function(url:String, options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest {})
	static function request(options:HttpsRequestOptions, ?callback:IncomingMessage->Void):ClientRequest;
}

typedef HttpsCreateServerOptions = {
	> js.node.Tls.TlsCreateServerOptions,
	> js.node.tls.SecureContext.SecureContextOptions,
	> js.node.Http.HttpCreateServerOptions,
}

typedef HttpsRequestOptions = {
	> js.node.Http.HttpRequestOptions,
	> js.node.Tls.TlsConnectOptions,
	// TODO: clean those options up
}
