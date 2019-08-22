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

// This file is generated from mozilla\Clipboard.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `Clipboard` interface implements the Clipboard API, providing—if the user grants permission—both read and write access to the contents of the system clipboard.

	Documentation [Clipboard](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Clipboard>
**/
@:native("Clipboard")
extern class Clipboard extends EventTarget {
	
	/**
		Requests arbitrary data (such as images) from the clipboard, returning a `Promise`. When the data has been retrieved, the promise is resolved with a `DataTransfer` object that provides the data.
		@throws DOMError
	**/
	function read() : Promise<DataTransfer>;
	
	/**
		Requests text from the system clipboard; returns a `Promise` which is resolved with a `DOMString` containing the clipboard's text once it's available.
		@throws DOMError
	**/
	function readText() : Promise<String>;
	
	/**
		Writes arbitrary data to the system clipboard. This asynchronous operation signals that it's finished by resolving the returned `Promise`.
		@throws DOMError
	**/
	function write( data : DataTransfer ) : Promise<Void>;
	
	/**
		Writes text to the system clipboard, returning a `Promise` which is resolved once the text is fully copied into the clipboard.
		@throws DOMError
	**/
	function writeText( data : String ) : Promise<Void>;
}