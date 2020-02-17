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

// This file is generated from mozilla\StorageManager.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `StorageManager` interface of the the Storage API provides an interface for managing persistance permissions and estimating available storage. You can get a reference to this interface using either `navigator.storage` or `WorkerNavigator.storage`.

	Documentation [StorageManager](https://developer.mozilla.org/en-US/docs/Web/API/StorageManager) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/StorageManager$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/StorageManager>
**/
@:native("StorageManager")
extern class StorageManager {

	/**
		Returns a `Promise` that resolves to `true` if persistence has already been granted for your site's storage.
		@throws DOMError
	**/
	function persisted() : Promise<Bool>;

	/**
		Returns a `Promise` that resolves to `true` if the user agent is able to persist your site's storage.
		@throws DOMError
	**/
	function persist() : Promise<Bool>;

	/**
		Returns a `StorageEstimate` object containing usage and quota numbers for your origin.
		@throws DOMError
	**/
	function estimate() : Promise<StorageEstimate>;
}