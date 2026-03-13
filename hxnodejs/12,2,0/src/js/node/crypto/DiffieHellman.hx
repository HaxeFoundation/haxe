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
	Common interface for `DiffieHellman` and a mimic object returned by `Crypto.getDiffieHellman`.
	See `DiffieHellman` documentation.
**/
@:remove
extern interface IDiffieHellman {
	@:overload(function():Buffer {})
	function generateKeys(encoding:String):String;

	@:overload(function(other_public_key:Buffer):Buffer {})
	@:overload(function(other_public_key:String, input_encoding:String):Buffer {})
	function computeSecret(other_public_key:String, input_encoding:String, output_encoding:String):String;

	@:overload(function():Buffer {})
	function getPrime(encoding:String):String;

	@:overload(function():Buffer {})
	function getGenerator(encoding:String):String;

	@:overload(function():Buffer {})
	function getPublicKey(encoding:String):String;

	@:overload(function():Buffer {})
	function getPrivateKey(encoding:String):String;
}

/**
	The class for creating Diffie-Hellman key exchanges.
	Returned by `Crypto.createDiffieHellman`.
**/
extern class DiffieHellman implements IDiffieHellman {
	/**
		Generates private and public Diffie-Hellman key values, and returns the public key in the specified `encoding`.
		This key should be transferred to the other party. `encoding` can be 'binary', 'hex', or 'base64'.
	**/
	@:overload(function():Buffer {})
	function generateKeys(encoding:String):String;

	/**
		Computes the shared secret using `other_public_key` as the other party's public key
		and returns the computed shared secret.

		Supplied key is interpreted using specified `input_encoding`,
		and secret is encoded using specified `output_encoding`.

		Encodings can be 'binary', 'hex', or 'base64'.

		If the input encoding is not provided, then a buffer is expected.
	**/
	@:overload(function(other_public_key:Buffer):Buffer {})
	@:overload(function(other_public_key:String, input_encoding:String):Buffer {})
	function computeSecret(other_public_key:String, input_encoding:String, output_encoding:String):String;

	/**
		Returns the Diffie-Hellman prime in the specified encoding, which can be 'binary', 'hex', or 'base64'.
		If no encoding is provided, then a buffer is returned.
	**/
	@:overload(function():Buffer {})
	function getPrime(encoding:String):String;

	/**
		Returns the Diffie-Hellman generator in the specified encoding, which can be 'binary', 'hex', or 'base64'.
		If no encoding is provided, then a buffer is returned.
	**/
	@:overload(function():Buffer {})
	function getGenerator(encoding:String):String;

	/**
		Returns the Diffie-Hellman public key in the specified encoding, which can be 'binary', 'hex', or 'base64'.
		If no encoding is provided, then a buffer is returned.
	**/
	@:overload(function():Buffer {})
	function getPublicKey(encoding:String):String;

	/**
		Returns the Diffie-Hellman private key in the specified encoding, which can be 'binary', 'hex', or 'base64'.
		If no encoding is provided, then a buffer is returned.
	**/
	@:overload(function():Buffer {})
	function getPrivateKey(encoding:String):String;

	/**
		Sets the Diffie-Hellman public key. Key encoding can be 'binary', 'hex' or 'base64'.
		If no `encoding` is provided, then a `Buffer` is expected.
	**/
	@:overload(function(public_key:Buffer):Void {})
	function setPublicKey(public_key:String, encoding:String):Void;

	/**
		Sets the Diffie-Hellman private key. Key encoding can be 'binary', 'hex' or 'base64'.
		If no `encoding` is provided, then a `Buffer` is expected.
	**/
	@:overload(function(private_key:Buffer):Void {})
	function setPrivateKey(private_key:String, encoding:String):Void;
}
