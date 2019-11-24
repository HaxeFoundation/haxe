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

// This file is generated from mozilla\PushManager.webidl. Do not edit!

package js.html.push;

import js.lib.Promise;

/**
	The `PushManager` interface of the Push API provides a way to receive notifications from third-party servers as well as request URLs for push notifications.

	Documentation [PushManager](https://developer.mozilla.org/en-US/docs/Web/API/PushManager) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PushManager$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PushManager>
**/
@:native("PushManager")
extern class PushManager {
	/** @throws DOMError */
	function new( scope : String ) : Void;

	/**
		Subscribes to a push service. It returns a `Promise` that resolves to a `PushSubscription` object containing details of a push subscription. A new push subscription is created if the current service worker does not have an existing subscription.
		@throws DOMError
	**/
	function subscribe( ?options : PushSubscriptionOptionsInit ) : Promise<PushSubscription>;

	/**
		Retrieves an existing push subscription. It returns a `Promise` that resolves to a `PushSubscription` object containing details of an existing subscription. If no existing subscription exists, this resolves to a `null` value.
		@throws DOMError
	**/
	function getSubscription() : Promise<PushSubscription>;

	/**
		Returns a `Promise` that resolves to the permission state of the current `PushManager`, which will be one of `'granted'`, `'denied'`, or `'prompt'`.
		@throws DOMError
	**/
	function permissionState( ?options : PushSubscriptionOptionsInit ) : Promise<PushPermissionState>;
}