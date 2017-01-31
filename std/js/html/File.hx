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

// This file is generated from mozilla\File.webidl. Do not edit!

package js.html;

/**
	The `File` interface provides information about files and allows JavaScript in a web page to access their content.

	Documentation [File](https://developer.mozilla.org/en-US/docs/Web/API/File) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/File$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/File>
**/
@:native("File")
extern class File extends Blob
{
	
	/**
		Returns the name of the file referenced by the `File` object.
	**/
	var name(default,null) : String;
	
	/**
		Returns the last modified time of the file, in millisecond since the UNIX epoch (January 1st, 1970 at Midnight).
	**/
	var lastModified(default,null) : Int;
	
	/**
		Returns the last modified `Date` of the file referenced by the `File` object.
	**/
	var lastModifiedDate(default,null) : Date;
	
	/** @throws DOMError */
	@:overload( function( fileBits : Array<haxe.extern.EitherType<ArrayBuffer,haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,String>>>>, fileName : String, ?options : FilePropertyBag ) : Void {} )
	@:overload( function( fileBits : Blob, ?options : ChromeFilePropertyBag ) : Void {} )
	@:overload( function( fileBits : Dynamic/*MISSING nsIFile*/, ?options : ChromeFilePropertyBag ) : Void {} )
	function new( fileBits : String, ?options : ChromeFilePropertyBag ) : Void;
}