/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	The `CryptoKey` interface represents a cryptographic key derived from a specific key algorithm.

	Documentation [CryptoKey](https://developer.mozilla.org/en-US/docs/Web/API/CryptoKey) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CryptoKey$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CryptoKey>
**/
@:native("CryptoKey")
extern class CryptoKey
{
	
	/**
		Returns an enumerated value representing the type of the key, a secret key (for symmetric algorithm), a public or a private key (for an asymmetric algorithm)
	**/
	var type(default,null) : String;
	
	/**
		Returns a `Boolean` indicating if the raw information may be exported to the application or not.
	**/
	var extractable(default,null) : Bool;
	
	/**
		Returns an opaque object representing a particular cipher the key has to be used with.
	**/
	var algorithm(default,null) : Dynamic;
	
	/**
		Returns an array of enumerated values indicating what the key can be used for.
	**/
	var usages(default,null) : Array<String>;
	
}