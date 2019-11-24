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

// This file is generated from mozilla\DataTransferItemList.webidl. Do not edit!

package js.html;

/**
	The `DataTransferItemList` object is a list of `DataTransferItem` objects representing items being dragged. During a drag operation, each `DragEvent` has a `dataTransfer` property and that property is a `DataTransferItemList`.

	Documentation [DataTransferItemList](https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItemList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItemList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItemList>
**/
@:native("DataTransferItemList")
extern class DataTransferItemList implements ArrayAccess<DataTransferItem> {
	
	/**
		An `unsigned long` that is the number of drag items in the list.
	**/
	var length(default,null) : Int;
	
	
	/**
		Adds an item (either a `File` object or a `DOMString`) to the drag item list and returns a `DataTransferItem` object for the new item.
		@throws DOMError
	**/
	@:overload( function( data : String, type : String ) : DataTransferItem {} )
	function add( data : File ) : DataTransferItem;
	
	/**
		Removes the drag item from the list at the given index.
		@throws DOMError
	**/
	function remove( index : Int ) : Void;
	
	/**
		Removes all of the drag items from the list.
		@throws DOMError
	**/
	function clear() : Void;
}