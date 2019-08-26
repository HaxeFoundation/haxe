/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\PushSubscription.webidl. Do not edit!

package js.html.push;

import js.lib.Promise;

/**
	The `PushSubscription` interface of the Push API provides a subcription's URL endpoint and allows unsubscription from a push service.

	Documentation [PushSubscription](https://developer.mozilla.org/en-US/docs/Web/API/PushSubscription) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PushSubscription$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PushSubscription>
**/
@:native("PushSubscription")
extern class PushSubscription {

	/**
		A `USVString` containing the endpoint associated with the push subscription.
	**/
	var endpoint(default,null) : String;

	/**
		An object containing the options used to create the subscription.
	**/
	var options(default,null) : PushSubscriptionOptions;

	/** @throws DOMError */
	function new( initDict : PushSubscriptionInit ) : Void;

	/**
		Returns an `ArrayBuffer` which contains the client's public key, which can then be sent to a server and used in encrypting push message data.
		@throws DOMError
	**/
	function getKey( name : PushEncryptionKeyName ) : js.lib.ArrayBuffer;

	/**
		Starts the asynchronous process of unsubscribing from the push service, returning a `Promise` that resolves to a `Boolean` when the current subscription is successfully unregistered.
		@throws DOMError
	**/
	function unsubscribe() : Promise<Bool>;

	/**
		Standard serializer — returns a JSON representation of the subscription properties.
		@throws DOMError
	**/
	function toJSON() : PushSubscriptionJSON;
}