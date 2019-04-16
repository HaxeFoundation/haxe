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

// This file is generated from mozilla\DataTransferItem.webidl. Do not edit!

package js.html;

/**
	The `DataTransferItem` object represents one drag data item. During a drag operation, each `drag event` has a `dataTransfer` property which contains a `list` of drag data items. Each item in the list is a `DataTransferItem` object.

	Documentation [DataTransferItem](https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItem) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItem$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItem>
**/
@:native("DataTransferItem")
extern class DataTransferItem {
	
	/**
		The kind of drag data item, `string` or `file`.
	**/
	var kind(default,null) : String;
	
	/**
		The drag data item's type, typically a MIME type.
	**/
	var type(default,null) : String;
	
	
	/**
		Invokes the specified callback with the drag data item string as its argument.
		@throws DOMError
	**/
	function getAsString( callback : String -> Void ) : Void;
	
	/**
		Returns the `File` object associated with the drag data item (or null if the drag item is not a file).
		@throws DOMError
	**/
	function getAsFile() : File;
}