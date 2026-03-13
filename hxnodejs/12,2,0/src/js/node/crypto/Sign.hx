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

import haxe.extern.EitherType;
import js.node.Buffer;
import js.node.stream.Writable;

/**
	Class for generating signatures.

	Returned by `Crypto.createSign`.

	Sign objects are writable streams. The written data is used to generate the signature.
	Once all of the data has been written, the sign method will return the signature.

	The legacy `update` method is also supported.
**/
extern class Sign extends Writable<Sign> {
	/**
		Updates the sign object with data.
		This can be called many times with new data as it is streamed.
	**/
	@:overload(function(data:Buffer):Void {})
	function update(data:String, ?encoding:String):Void;

	/**
		Calculates the signature on all the updated data passed through the sign.
		`private_key` is a string containing the PEM encoded private key for signing.
		Returns the signature in `output_format` which can be 'binary', 'hex' or 'base64'.
		If no encoding is provided, then a buffer is returned.

		Note: sign object can not be used after `sign` method has been called.
	**/
	@:overload(function(private_key:EitherType<String, {key:String, passphrase:String}>):Buffer {})
	function sign(private_key:EitherType<String, {key:String, passphrase:String}>, output_format:String):String;
}
