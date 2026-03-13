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
	Class for encrypting data.

	Returned by `Crypto.createCipher` and `Crypto.createCipheriv`.

	Cipher objects are streams that are both readable and writable.
	The written plain text data is used to produce the encrypted data on the readable side.

	The legacy `update` and `final` methods are also supported.
**/
extern class Cipher extends js.node.stream.Transform<Cipher> {
	/**
		Updates the cipher with `data`, the encoding of which is given in `input_encoding`
		and can be 'utf8', 'ascii' or 'binary'. If no encoding is provided, then a buffer is expected.
		If data is a Buffer then `input_encoding` is ignored.

		The `output_encoding` specifies the output format of the enciphered data,
		and can be 'binary', 'base64' or 'hex'. If no encoding is provided, then a buffer is returned.

		Returns the enciphered contents, and can be called many times with new data as it is streamed.
	**/
	@:overload(function(data:Buffer):Buffer {})
	@:overload(function(data:String, input_encoding:String):Buffer {})
	function update(data:String, input_encoding:String, output_encoding:String):String;

	/**
		Returns any remaining enciphered contents, with `output_encoding` being one of: 'binary', 'base64' or 'hex'.
		If no encoding is provided, then a buffer is returned.

		Note: cipher object can not be used after `final` method has been called.
	**/
	@:native("final") @:overload(function():Buffer {})
	function finalContents(output_encoding:String):String;

	/**
		You can disable automatic padding of the input data to block size.
		If `auto_padding` is false, the length of the entire input data
		must be a multiple of the cipher's block size or `final` will fail.

		Useful for non-standard padding, e.g. using 0x0 instead of PKCS padding.
		You must call this before `final`.
	**/
	@:overload(function():Void {})
	function setAutoPadding(auto_padding:Bool):Void;

	/**
		For authenticated encryption modes (currently supported: GCM), this method returns a `Buffer`
		that represents the authentication tag that has been computed from the given data.
		Should be called after encryption has been completed using the `final` method!
	**/
	function getAuthTag():Buffer;

	/**
		For authenticated encryption modes (currently supported: GCM), this method sets the value
		used for the additional authenticated data (AAD) input parameter.
	**/
	function setAAD(buffer:Buffer):Void;
}
