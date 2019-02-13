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

/**
	The `StorageEstimate` dictionary is used by the `StorageManager` to provide estimates of the size of a site's or application's data store and how much of it is in use. The `estimate()` method returns an object that conforms to this dictionary when its `Promise` resolves.

	Documentation [StorageEstimate](https://developer.mozilla.org/en-US/docs/Web/API/StorageEstimate) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/StorageEstimate$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/StorageEstimate>
**/
typedef StorageEstimate = {
	
	/**
		A numeric value which provides a conservative approximation of the total storage the user's device or computer has available for the site origin or Web app. It's possible that there's more than this amount of space available though you can't rely on that being the case.
	**/
	var ?quota : Int;
	
	/**
		A numeric value approximating the amount of storage space currently being used by the site or Web app, out of the available space as indicated by `quota`.
	**/
	var ?usage : Int;
}