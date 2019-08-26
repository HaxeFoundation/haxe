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

// This file is generated from mozilla\SubtleCrypto.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `SubtleCrypto` interface represents a set of cryptographic primitives. It is available via the `Crypto.subtle` properties available in a window context (via `Window.crypto`).

	Documentation [SubtleCrypto](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto>
**/
@:native("SubtleCrypto")
extern class SubtleCrypto {

	/**
		Returns a `Promise` of the encrypted data corresponding to the clear text, algorithm and key given as parameters.
		@throws DOMError
	**/
	function encrypt( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of the clear data corresponding to the encrypted text, algorithm and key given as parameters.
		@throws DOMError
	**/
	function decrypt( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of the signature corresponding to the text, algorithm and key given as parameters.
		@throws DOMError
	**/
	function sign( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a `Boolean` value indicating if the signature given as parameter matches the text, algorithm and key also given as parameters.
		@throws DOMError
	**/
	function verify( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, signature : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer>, data : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a digest generated from the algorithm and text given as parameters.
		@throws DOMError
	**/
	function digest( algorithm : haxe.extern.EitherType<Dynamic,String>, data : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a newly generated `CryptoKey`, for symmetrical algorithms, or a `CryptoKeyPair`, containing two newly generated keys, for asymmetrical algorithm, that matches the algorithm, the usages and the extractability given as parameters.
		@throws DOMError
	**/
	@:overload( function( algorithm : String, extractable : Bool, keyUsages : Array<String>) : Promise<Dynamic> {} )
	function generateKey( algorithm : Dynamic, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a newly generated `CryptoKey` derived from a master key and a specific algorithm given as parameters.
		@throws DOMError
	**/
	function deriveKey( algorithm : haxe.extern.EitherType<Dynamic,String>, baseKey : CryptoKey, derivedKeyType : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a newly generated buffer of pseudo-random bits derived from a master key and a specific algorithm given as parameters.
		@throws DOMError
	**/
	@:overload( function( algorithm : String, baseKey : CryptoKey, length : Int) : Promise<Dynamic> {} )
	function deriveBits( algorithm : Dynamic, baseKey : CryptoKey, length : Int ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a `CryptoKey` corresponding to the format, the algorithm, the raw key data, the usages and the extractability given as parameters.
		@throws DOMError
	**/
	@:overload( function( format : String, keyData : Dynamic, algorithm : String, extractable : Bool, keyUsages : Array<String>) : Promise<Dynamic> {} )
	function importKey( format : String, keyData : Dynamic, algorithm : Dynamic, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a buffer containing the key in the format requested.
		@throws DOMError
	**/
	function exportKey( format : String, key : CryptoKey ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a wrapped symmetric key for usage (transfer, storage) in insecure environments. The wrapped buffer returned is in the format given in parameters, and contains the key wrapped by the given wrapping key with the given algorithm.
		@throws DOMError
	**/
	@:overload( function( format : String, key : CryptoKey, wrappingKey : CryptoKey, wrapAlgorithm : String) : Promise<Dynamic> {} )
	function wrapKey( format : String, key : CryptoKey, wrappingKey : CryptoKey, wrapAlgorithm : Dynamic ) : Promise<Dynamic>;

	/**
		Returns a `Promise` of a `CryptoKey` corresponding to the wrapped key given in parameter.
		@throws DOMError
	**/
	function unwrapKey( format : String, wrappedKey : haxe.extern.EitherType<js.lib.ArrayBufferView,js.lib.ArrayBuffer>, unwrappingKey : CryptoKey, unwrapAlgorithm : haxe.extern.EitherType<Dynamic,String>, unwrappedKeyAlgorithm : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;
}