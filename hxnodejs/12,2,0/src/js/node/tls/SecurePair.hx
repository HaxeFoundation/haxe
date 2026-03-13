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

import js.node.events.EventEmitter;

/**
	Events emitted by `SecurePair`.
**/
enum abstract SecurePairEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		The event is emitted from the `SecurePair` once the pair has successfully established a secure connection.

		Similarly to the checking for the server 'secureConnection' event,
		`SecurePair.cleartext.authorized` should be checked to confirm whether
		the certificate used properly authorized.
	**/
	var Secure:SecurePairEvent<Void->Void> = "secure";
}

/**
	Returned by `Tls.createSecurePair`.
**/
extern class SecurePair extends EventEmitter<SecurePair> {
	var cleartext(default, null):TLSSocket;
	var encrypted(default, null):js.node.stream.Duplex.IDuplex;
}
