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

// This file is generated from mozilla\Selection.webidl. Do not edit!

package js.html;

/**
	A `Selection` object represents the range of text selected by the user or the current position of the caret. To obtain a Selection object for examination or modification, call `window.getSelection()`.

	Documentation [Selection](https://developer.mozilla.org/en-US/docs/Web/API/Selection) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Selection$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Selection>
**/
@:native("Selection")
extern class Selection {
	
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
	
	/**
		Returns a `DOMString` describing the type of the current selection.
	**/
	var type(default,null) : String;
	var caretBidiLevel : Int;
	
	
	/**
		Returns a `Range` object representing one of the ranges currently selected.
		@throws DOMError
	**/
	function getRangeAt( index : Int ) : Range;
	
	/**
		A `Range` object that will be added to the selection.
		@throws DOMError
	**/
	function addRange( range : Range ) : Void;
	
	/**
		Removes a range from the selection.
		@throws DOMError
	**/
	function removeRange( range : Range ) : Void;
	
	/**
		Removes all ranges from the selection.
		@throws DOMError
	**/
	function removeAllRanges() : Void;
	/** @throws DOMError */
	function empty() : Void;
	
	/**
		Collapses the current selection to a single point.
		@throws DOMError
	**/
	function collapse( node : Node, offset : Int = 0 ) : Void;
	/** @throws DOMError */
	function setPosition( node : Node, offset : Int = 0 ) : Void;
	
	/**
		Collapses the selection to the start of the first range in the selection.
		@throws DOMError
	**/
	function collapseToStart() : Void;
	
	/**
		Collapses the selection to the end of the last range in the selection.
		@throws DOMError
	**/
	function collapseToEnd() : Void;
	
	/**
		Moves the focus of the selection to a specified point.
		@throws DOMError
	**/
	function extend( node : Node, offset : Int = 0 ) : Void;
	
	/**
		Sets the selection to be a range including all or parts of two specified DOM nodes, and any content located between them.
		@throws DOMError
	**/
	function setBaseAndExtent( anchorNode : Node, anchorOffset : Int, focusNode : Node, focusOffset : Int ) : Void;
	
	/**
		Adds all the children of the specified node to the selection.
		@throws DOMError
	**/
	function selectAllChildren( node : Node ) : Void;
	
	/**
		Deletes the selection's content from the document.
		@throws DOMError
	**/
	function deleteFromDocument() : Void;
	
	/**
		Indicates if a certain node is part of the selection.
		@throws DOMError
	**/
	function containsNode( node : Node, allowPartialContainment : Bool = false ) : Bool;
	
	/**
		Changes the current selection.
		@throws DOMError
	**/
	function modify( alter : String, direction : String, granularity : String ) : Void;
}