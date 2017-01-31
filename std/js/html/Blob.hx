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

// This file is generated from mozilla\Blob.webidl. Do not edit!

package js.html;

/**
	A `Blob` object represents a file-like object of immutable, raw data. Blobs represent data that isn't necessarily in a JavaScript-native format. The `File` interface is based on `Blob`, inheriting blob functionality and expanding it to support files on the user's system.

	Documentation [Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Blob$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Blob>
**/
@:native("Blob")
extern class Blob
{
	
	/**
		The size, in bytes, of the data contained in the `Blob` object.
	**/
	var size(default,null) : Int;
	
	/**
		A string indicating the MIME type of the data contained in the `Blob`. If the type is unknown, this string is empty.
	**/
	var type(default,null) : String;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function new( blobParts : Array<haxe.extern.EitherType<ArrayBuffer,haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,String>>>>, ?options : BlobPropertyBag ) : Void;
	/** @throws DOMError */
	
	/**
		Returns a new `Blob` object containing the data in the specified range of bytes of the source `Blob`.
	**/
	function slice( ?start : Int, ?end : Int, ?contentType : String = "" ) : Blob;
}