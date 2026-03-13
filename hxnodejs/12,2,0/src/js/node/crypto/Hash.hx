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
	The class for creating hash digests of data.

	It is a stream that is both readable and writable.
	The written data is used to compute the hash.
	Once the writable side of the stream is ended, use the `read` method to get the computed hash digest.

	The legacy `update` and `digest` methods are also supported.

	Returned by `Crypto.createHash`.
**/
extern class Hash extends js.node.stream.Transform<Hash> {
	/**
		Updates the hash content with the given `data`,

		the `encoding` of which is given in `input_encoding` and can be 'utf8', 'ascii' or 'binary'.
		If no `encoding` is provided and the input is a string an encoding of 'binary' is enforced.
		If `data` is a `Buffer` then `input_encoding` is ignored.

		This can be called many times with new data as it is streamed.
	**/
	@:overload(function(data:Buffer):Hash {})
	function update(data:String, ?input_encoding:String):Hash;

	/**
		Calculates the digest of all of the passed data to be hashed.
		The `encoding` can be 'hex', 'binary' or 'base64'.
		If no `encoding` is provided, then a buffer is returned.

		Note: hash object can not be used after `digest` method has been called.
	**/
	@:overload(function():Buffer {})
	function digest(encoding:String):String;
}
