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

// This file is generated from mozilla\FileReader.webidl. Do not edit!

package js.html;

/**
	The `FileReader` object lets web applications asynchronously read the contents of files (or raw data buffers) stored on the user's computer, using `File` or `Blob` objects to specify the file or data to read.

	Documentation [FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FileReader$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FileReader>
**/
@:native("FileReader")
extern class FileReader extends EventTarget
{
	static inline var EMPTY : Int = 0;
	static inline var LOADING : Int = 1;
	static inline var DONE : Int = 2;
	
	
	/**
		A number indicating the state of the <code>FileReader</code>. This is one of the following:
		 <table class="standard-table">
		  
		   <tr>
		    <td><code>EMPTY</code></td>
		    <td><code>0</code></td>
		    <td>No data has been loaded yet.</td>
		   </tr>
		   <tr>
		    <td><code>LOADING</code></td>
		    <td><code>1</code></td>
		    <td>Data is currently being loaded.</td>
		   </tr>
		   <tr>
		    <td><code>DONE</code></td>
		    <td><code>2</code></td>
		    <td>The entire read request has been completed.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var readyState(default,null) : Int;
	
	/**
		The file's contents. This property is only valid after the read operation is complete, and the format of the data depends on which of the methods was used to initiate the read operation.
	**/
	var result(default,null) : Dynamic;
	
	/**
		A `DOMError` representing the error that occurred while reading the file.
	**/
	var error(default,null) : DOMError;
	
	/**
		A handler for the `loadstart` event. This event is triggered each time the reading is starting.
	**/
	var onloadstart : haxe.Constraints.Function;
	
	/**
		A handler for the `progress` event. This event is triggered while reading a `Blob` content.
	**/
	var onprogress : haxe.Constraints.Function;
	
	/**
		A handler for the `load` event. This event is triggered each time the reading operation is successfully completed.
	**/
	var onload : haxe.Constraints.Function;
	
	/**
		A handler for the `abort` event. This event is triggered each time the reading operation is aborted.
	**/
	var onabort : haxe.Constraints.Function;
	
	/**
		A handler for the `error` event. This event is triggered each time the reading operation encounter an error.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		A handler for the `loadend` event. This event is triggered each time the reading operation is completed (either in success or failure).
	**/
	var onloadend : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	
	/**
		Starts reading the contents of the specified `Blob`, once finished, the `result` attribute contains an `ArrayBuffer` representing the file's data.
	**/
	function readAsArrayBuffer( blob : Blob ) : Void;
	/** @throws DOMError */
	
	/**
		Starts reading the contents of the specified `Blob`, once finished, the `result` attribute contains the contents of the file as a text string.
	**/
	function readAsText( blob : Blob, ?label : String = "" ) : Void;
	/** @throws DOMError */
	
	/**
		Starts reading the contents of the specified `Blob`, once finished, the `result` attribute contains a `data:` URL representing the file's data.
	**/
	function readAsDataURL( blob : Blob ) : Void;
	/** @throws DOMError */
	
	/**
		Aborts the read operation. Upon return, the `readyState` will be `DONE`.
	**/
	function abort() : Void;
	/** @throws DOMError */
	
	/**
		Starts reading the contents of the specified `Blob`, once finished, the `result` attribute contains the raw binary data from the file as a string.
	**/
	function readAsBinaryString( filedata : Blob ) : Void;
}