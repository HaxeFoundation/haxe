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

package js.node.crypto;

import js.node.Buffer;

/**
	Class for creating cryptographic hmac content.

	It is a stream that is both readable and writable. The written data is used to compute the hmac.
	Once the writable side of the stream is ended, use the `read` method to get the computed digest.

	The legacy `update` and `digest` methods are also supported.

	Returned by `Crypto.createHmac`.
**/
extern class Hmac extends js.node.stream.Transform<Hmac> {
	/**
		Update the hmac content with the given data.

		This can be called many times with new data as it is streamed.
	**/
	@:overload(function(data:Buffer):Hmac {})
	function update(data:String, ?input_encoding:String):Hmac;

	/**
		Calculates the digest of all of the passed data to the hmac.
		The `encoding` can be 'hex', 'binary' or 'base64'.
		If no `encoding` is provided, then a buffer is returned.

		Note: hmac object can not be used after `digest` method has been called.
	**/
	@:overload(function():Buffer {})
	function digest(encoding:String):String;
}
