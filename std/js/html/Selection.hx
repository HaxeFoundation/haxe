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

// This file is generated from mozilla\Selection.webidl. Do not edit!

package js.html;

/**
	Calling the `Selection.toString()` method returns the text contained in the selection, e.g.:

	Documentation [Selection](https://developer.mozilla.org/en-US/docs/Web/API/Selection) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Selection$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Selection>
**/
@:native("Selection")
extern class Selection
{
	
	/**
		Returns the `Node` in which the selection begins.
	**/
	var anchorNode(default,null) : Node;
	
	/**
		Returns a number representing the offset of the selection's anchor within the anchorNode. If anchorNode is a text node, this is the number of characters within anchorNode preceding the anchor. If anchorNode is an element, this is the number of child nodes of the anchorNode preceding the anchor.
	**/
	var anchorOffset(default,null) : Int;
	
	/**
		Returns the `Node` in which the selection ends.
	**/
	var focusNode(default,null) : Node;
	
	/**
		Returns a number representing the offset of the selection's anchor within the focusNode. If focusNode is a text node, this is the number of characters within focusNode preceding the focus. If focusNode is an element, this is the number of child nodes of the focusNode preceding the focus.
	**/
	var focusOffset(default,null) : Int;
	
	/**
		Returns a Boolean indicating whether the selection's start and end points are at the same position.
	**/
	var isCollapsed(default,null) : Bool;
	
	/**
		Returns the number of ranges in the selection.
	**/
	var rangeCount(default,null) : Int;
	var caretBidiLevel : Int;
	
	/** @throws DOMError */
	
	/**
		Collapses the current selection to a single point.
	**/
	function collapse( node : Node, offset : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Collapses the selection to the start of the first range in the selection.
	**/
	function collapseToStart() : Void;
	/** @throws DOMError */
	
	/**
		Collapses the selection to the end of the last range in the selection.
	**/
	function collapseToEnd() : Void;
	/** @throws DOMError */
	
	/**
		Moves the focus of the selection to a specified point.
	**/
	function extend( node : Node, offset : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Adds all the children of the specified node to the selection.
	**/
	function selectAllChildren( node : Node ) : Void;
	/** @throws DOMError */
	
	/**
		Deletes the selection's content from the document.
	**/
	function deleteFromDocument() : Void;
	/** @throws DOMError */
	
	/**
		Returns a `Range` object representing one of the ranges currently selected.
	**/
	function getRangeAt( index : Int ) : Range;
	/** @throws DOMError */
	
	/**
		A `Range` object that will be added to the selection.
	**/
	function addRange( range : Range ) : Void;
	/** @throws DOMError */
	
	/**
		Removes a range from the selection.
	**/
	function removeRange( range : Range ) : Void;
	/** @throws DOMError */
	
	/**
		Removes all ranges from the selection.
	**/
	function removeAllRanges() : Void;
	/** @throws DOMError */
	
	/**
		Indicates if a certain node is part of the selection.
	**/
	function containsNode( node : Node, allowPartialContainment : Bool ) : Bool;
	/** @throws DOMError */
	
	/**
		Changes the current selection.
	**/
	function modify( alter : String, direction : String, granularity : String ) : Void;
}