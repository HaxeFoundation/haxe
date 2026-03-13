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
	Class for decrypting data.

	Returned by `Crypto.createDecipher` and `Crypto.createDecipheriv`.

	Decipher objects are streams that are both readable and writable.
	The written enciphered data is used to produce the plain-text data on the the readable side.

	The legacy `update` and `final` methods are also supported.
**/
extern class Decipher extends js.node.stream.Transform<Decipher> {
	/**
		Updates the decipher with `data`, which is encoded in 'binary', 'base64' or 'hex'.
		If no encoding is provided, then a buffer is expected.

		The `output_decoding` specifies in what format to return the deciphered plaintext: 'binary', 'ascii' or 'utf8'.
		If no encoding is provided, then a buffer is returned.
	**/
	@:overload(function(data:Buffer):Buffer {})
	@:overload(function(data:String, input_encoding:String):Buffer {})
	function update(data:String, input_encoding:String, output_encoding:String):String;

	/**
		Returns any remaining plaintext which is deciphered,
		with `output_encoding` being one of: 'binary', 'ascii' or 'utf8'.
		If no encoding is provided, then a buffer is returned.

		Note: decipher object can not be used after `final` method has been called.
	**/
	@:native("final") @:overload(function():Buffer {})
	function finalContents(output_encoding:String):String;

	/**
		You can disable auto padding if the data has been encrypted without standard block padding
		to prevent `final` from checking and removing it.

		Can only work if the input data's length is a multiple of the ciphers block size.

		You must call this before streaming data to `update`.
	**/
	@:overload(function():Void {})
	function setAutoPadding(auto_padding:Bool):Void;

	/**
		For authenticated encryption modes (currently supported: GCM), this method must be used
		to pass in the received authentication tag. If no tag is provided or if the ciphertext
		has been tampered with, `final` will throw, thus indicating that the ciphertext should be
		discarded due to failed authentication.
	**/
	function setAuthTag(buffer:Buffer):Void;

	/**
		For authenticated encryption modes (currently supported: GCM), this method sets the value
		used for the additional authenticated data (AAD) input parameter.
	**/
	function setAAD(buffer:Buffer):Void;
}
