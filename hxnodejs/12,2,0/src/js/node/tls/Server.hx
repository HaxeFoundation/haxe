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

import js.node.Buffer;
import js.node.events.EventEmitter.Event;
import js.node.tls.SecureContext.SecureContextOptions;
import js.node.tls.TLSSocket;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by `Server` in addition to its parent classes.
**/
enum abstract ServerEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		This event is emitted after a new connection has been successfully handshaked.
	**/
	var SecureConnection:ServerEvent<TLSSocket->Void> = "secureConnection";

	/**
		When a client connection emits an 'error' event before secure connection is established -
		it will be forwarded here.

		Listener arguments:
			exception - error object
			securePair - the `TLSSocket` that the error originated from
	**/
	var ClientError:ServerEvent<Error->TLSSocket->Void> = "clientError";

	/**
		Emitted on creation of TLS session.
		May be used to store sessions in external storage.

		`callback` must be invoked eventually, otherwise no data will be sent or received from secure connection.

		Listener arguments:
			sessionId
			sessionData
			callback
	**/
	var NewSession:ServerEvent<Buffer->Buffer->(Void->Void)->Void> = "newSession";

	/**
		Emitted when client wants to resume previous TLS session.

		Event listener may perform lookup in external storage using given sessionId,
		and invoke callback(null, sessionData) once finished.

		If session can't be resumed (i.e. doesn't exist in storage) one may call callback(null, null).

		Calling callback(err) will terminate incoming connection and destroy socket.

		Listener arguments:
			sessionId
			callback
	**/
	var ResumeSession:ServerEvent<Buffer->(Error->?Buffer->Void)->Void> = "resumeSession";

	/**
		Emitted when the client sends a certificate status request.
		You could parse server's current certificate to obtain OCSP url and certificate id,
		and after obtaining OCSP response invoke `callback(null, resp)`, where `resp` is a `Buffer` instance.
		Both certificate and issuer are a Buffer DER-representations of the primary and issuer's certificates.
		They could be used to obtain OCSP certificate id and OCSP endpoint url.

		Alternatively, `callback(null, null)` could be called, meaning that there is no OCSP response.

		Calling `callback(err)` will result in a `socket.destroy(err)` call.
	**/
	var OCSPRequest:ServerEvent<Buffer->Buffer->(Error->?Buffer->Void)->Void> = "OCSPRequest";
}

/**
	This class is a subclass of `net.Server` and has the same methods on it.
	Instead of accepting just raw TCP connections, this accepts encrypted connections using TLS or SSL.
**/
@:jsRequire("tls", "Server")
extern class Server extends js.node.net.Server {
	/**
		Returns `Buffer` instance holding the keys currently used for encryption/decryption of the TLS Session Tickets.
	**/
	function getTicketKeys():Buffer;

	/**
		Updates the keys for encryption/decryption of the TLS Session Tickets.

		NOTE: the buffer should be 48 bytes long. See server `ticketKeys` option for
		more information on how it is going to be used.

		NOTE: the change is effective only for the future server connections. Existing or currently pending
		server connections will use previous keys.
	**/
	function setTicketKeys(keys:Buffer):Void;

	/**
		Add secure context that will be used if client request's SNI hostname
		is matching passed hostname (wildcards can be used).
	**/
	function addContext(hostname:String, credentials:SecureContextOptions):Void;
}
