/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/SubtleCrypto.webidl line 159:0. Do not edit!

package js.html;

@:native("SubtleCrypto")
extern class SubtleCrypto
{
	/** @throws DOMError */
	function encrypt( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function decrypt( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function sign( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, data : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function verify( algorithm : haxe.extern.EitherType<Dynamic,String>, key : CryptoKey, signature : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer>, data : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function digest( algorithm : haxe.extern.EitherType<Dynamic,String>, data : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function generateKey( algorithm : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function deriveKey( algorithm : haxe.extern.EitherType<Dynamic,String>, baseKey : CryptoKey, derivedKeyType : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function deriveBits( algorithm : haxe.extern.EitherType<Dynamic,String>, baseKey : CryptoKey, length : Int ) : Promise<Dynamic>;
	/** @throws DOMError */
	function importKey( format : String, keyData : Dynamic, algorithm : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function exportKey( format : String, key : CryptoKey ) : Promise<Dynamic>;
	/** @throws DOMError */
	function wrapKey( format : String, key : CryptoKey, wrappingKey : CryptoKey, wrapAlgorithm : haxe.extern.EitherType<Dynamic,String> ) : Promise<Dynamic>;
	/** @throws DOMError */
	function unwrapKey( format : String, wrappedKey : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer>, unwrappingKey : CryptoKey, unwrapAlgorithm : haxe.extern.EitherType<Dynamic,String>, unwrappedKeyAlgorithm : haxe.extern.EitherType<Dynamic,String>, extractable : Bool, keyUsages : Array<String> ) : Promise<Dynamic>;
}