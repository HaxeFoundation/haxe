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

package js.node.tls;

import haxe.Constraints.Function;
import js.node.Buffer;
import js.node.Tls.TlsClientOptionsBase;
import js.node.Tls.TlsServerOptionsBase;
import js.node.events.EventEmitter.Event;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by `TLSSocket` objects in addition to its parent class events.
**/
enum abstract TLSSocketEvent<T:Function>(Event<T>) to Event<T> {
	/**
		This event is emitted after a new connection has been successfully handshaked.

		The listener will be called no matter if the server's certificate was authorized or not.

		It is up to the user to test `TLSSocket.authorized` to see if the server certificate
		was signed by one of the specified CAs. If `TLSSocket.authorized` is false then the error
		can be found in `TLSSocket.authorizationError`. Also if NPN was used - you can
		check `TLSSocket.npnProtocol` for negotiated protocol.
	**/
	var SecureConnect:TLSSocketEvent<Void->Void> = "secureConnect";

	/**
		This event will be emitted if `requestOCSP` option was set.

		`response` is a `Buffer` object, containing server's OCSP response.

		Traditionally, the response is a signed object from the server's CA
		that contains information about server's certificate revocation status.
	**/
	var OCSPResponse:TLSSocketEvent<Buffer->Void> = "OCSPResponse";
}

typedef TLSSocketOptions = {
	> TlsServerOptionsBase,
	> TlsClientOptionsBase,

	/**
		An optional TLS context object from `Tls.createSecureContext`
	**/
	@:optional var secureContext:SecureContext;

	/**
		If true - TLS socket will be instantiated in server-mode
	**/
	@:optional var isServer:Bool;

	@:optional var server:js.node.net.Server;
}

/**
	This is a wrapped version of `net.Socket` that does transparent encryption
	of written data and all required TLS negotiation.

	Its `encrypted` field is always true.
**/
@:jsRequire("tls", "TLSSocket")
extern class TLSSocket extends js.node.net.Socket {
	/**
		Construct a new TLSSocket object from existing TCP socket.
	**/
	function new(socket:js.node.net.Socket, options:TLSSocketOptions);

	/**
		true if the peer certificate was signed by one of the specified CAs, otherwise false
	**/
	var authorized(default, null):Bool;

	/**
		The reason why the peer's certificate has not been verified.

		This property becomes available only when `authorized` is false.
	**/
	var authorizationError(default, null):Null<String>;

	/**
		Negotiated protocol name.
	**/
	var npnProtocol(default, null):String;

	/**
		Returns an object representing the peer's certificate.

		The returned object has some properties corresponding to the field of the certificate.
		If `detailed` argument is true - the full chain with issuer property will be returned,
		if false - only the top certificate without issuer property.
	**/
	function getPeerCertificate(?detailed:Bool):Dynamic; // TODO: is there a well defined structure for this?

	/**
		Returns an object representing the cipher name and the SSL/TLS protocol version of the current connection.

		Example: { name: 'AES256-SHA', version: 'TLSv1/SSLv3' }

		See SSL_CIPHER_get_name() and SSL_CIPHER_get_version() in http://www.openssl.org/docs/ssl/ssl.html#DEALING_WITH_CIPHERS for more information.
	**/
	function getCipher():{name:String, version:String};

	/**
		Initiate TLS renegotiation process.

		The `options` may contain the following fields: rejectUnauthorized, requestCert (See `Tls.createServer` for details).

		`callback(err)` will be executed with null as err, once the renegotiation is successfully completed.

		NOTE: Can be used to request peer's certificate after the secure connection has been established.
		ANOTHER NOTE: When running as the server, socket will be destroyed with an error after handshakeTimeout timeout.
	**/
	function renegotiate(options:{?rejectUnauthorized:Bool, ?requestCert:Bool}, ?callback:Error->Void):Bool;

	/**
		Set maximum TLS fragment size (default and maximum value is: 16384, minimum is: 512).

		Returns true on success, false otherwise.

		Smaller fragment size decreases buffering latency on the client: large fragments are buffered by the TLS layer
		until the entire fragment is received and its integrity is verified; large fragments can span multiple roundtrips,
		and their processing can be delayed due to packet loss or reordering. However, smaller fragments add
		extra TLS framing bytes and CPU overhead, which may decrease overall server throughput.
	**/
	function setMaxSendFragment(size:Int):Bool;

	/**
		Returns a string containing the negotiated SSL/TLS protocol version of the current connection.

		'unknown' will be returned for connected sockets that have not completed the handshaking process.
		`null` will be returned for server sockets or disconnected client sockets.
	**/
	function getProtocol():String;

	/**
		Return ASN.1 encoded TLS session or null if none was negotiated.
		Could be used to speed up handshake establishment when reconnecting to the server.
	**/
	function getSession():Null<Buffer>;

	/**
		NOTE: Works only with client TLS sockets.

		Useful only for debugging, for session reuse provide session option to tls.connect.

		Return TLS session ticket or null if none was negotiated.
	**/
	function getTLSTicket():Null<Buffer>;
}
