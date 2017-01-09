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

// This file is generated from mozilla\DataTransfer.webidl. Do not edit!

package js.html;

/**
	The `DataTransfer` object is used to hold the data that is being dragged during a drag and drop operation. It may hold one or more data items, each of one or more data types. For more information about drag and drop, see HTML Drag and Drop API.

	Documentation [DataTransfer](https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer>
**/
@:native("DataTransfer")
extern class DataTransfer
{
	
	/**
		Gets the type of drag-and-drop operation currently selected or sets the operation to a new type. The value must be `none`, `copy`, `link` or `move`.
	**/
	var dropEffect : String;
	
	/**
		Provides all of the types of operations that are possible. Must be one of `none`, `copy`, `copyLink`, `copyMove`, `link`, `linkMove`, `move`, `all` or `uninitialized`.
	**/
	var effectAllowed : String;
	
	/**
		An array of `DOMString` giving the formats that were set in the `dragstart` event.
	**/
	var types(default,null) : DOMStringList;
	
	/**
		Contains a list of all the local files available on the data transfer. If the drag operation doesn't involve dragging files, this property is an empty list.
	**/
	var files(default,null) : FileList;
	
	/** @throws DOMError */
	function new( eventType : String, isExternal : Bool ) : Void;
	/** @throws DOMError */
	
	/**
		Set the image to be used for dragging if a custom one is desired.
	**/
	function setDragImage( image : Element, x : Int, y : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Retrieves the data for a given type, or an empty string if data for that type does not exist or the data transfer contains no data.
	**/
	function getData( format : String ) : String;
	/** @throws DOMError */
	
	/**
		Set the data for a given type. If data for the type does not exist, it is added at the end, such that the last item in the types list will be the new format. If data for the type already exists, the existing data is replaced in the same position.
	**/
	function setData( format : String, data : String ) : Void;
	/** @throws DOMError */
	
	/**
		Remove the data associated with a given type. The type argument is optional. If the type is empty or not specified, the data associated with all types is removed. If data for the specified type does not exist, or the data transfer contains no data, this method will have no effect.
	**/
	function clearData( ?format : String ) : Void;
	/** @throws DOMError */
	
	/**
		Sets the drag source to the given element.
	**/
	function addElement( element : Element ) : Void;
}