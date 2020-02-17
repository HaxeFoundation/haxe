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

// This file is generated from mozilla\Range.webidl. Do not edit!

package js.html;

/**
	The `Range` interface represents a fragment of a document that can contain nodes and parts of text nodes.

	Documentation [Range](https://developer.mozilla.org/en-US/docs/Web/API/Range) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Range$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Range>
**/
@:native("Range")
extern class Range {
	static inline var START_TO_START : Int = 0;
	static inline var START_TO_END : Int = 1;
	static inline var END_TO_END : Int = 2;
	static inline var END_TO_START : Int = 3;
	
	
	/**
		Returns the `Node` within which the `Range` starts.
	**/
	var startContainer(default,null) : Node;
	
	/**
		Returns a number representing where in the `startContainer` the `Range` starts.
	**/
	var startOffset(default,null) : Int;
	
	/**
		Returns the `Node` within which the `Range` ends.
	**/
	var endContainer(default,null) : Node;
	
	/**
		Returns a number representing where in the `endContainer` the `Range` ends.
	**/
	var endOffset(default,null) : Int;
	
	/**
		Returns a `Boolean` indicating whether the range's start and end points are at the same position.
	**/
	var collapsed(default,null) : Bool;
	
	/**
		Returns the deepest `Node` that contains the `startContainer` and `endContainer` nodes.
	**/
	var commonAncestorContainer(default,null) : Node;
	
	/** @throws DOMError */
	function new() : Void;
	
	/**
		Sets the start position of a `Range`.
		@throws DOMError
	**/
	function setStart( refNode : Node, offset : Int ) : Void;
	
	/**
		Sets the end position of a `Range`.
		@throws DOMError
	**/
	function setEnd( refNode : Node, offset : Int ) : Void;
	
	/**
		Sets the start position of a `Range` relative to another `Node`.
		@throws DOMError
	**/
	function setStartBefore( refNode : Node ) : Void;
	
	/**
		Sets the start position of a `Range` relative to another `Node`.
		@throws DOMError
	**/
	function setStartAfter( refNode : Node ) : Void;
	
	/**
		Sets the end position of a `Range` relative to another `Node`.
		@throws DOMError
	**/
	function setEndBefore( refNode : Node ) : Void;
	
	/**
		Sets the end position of a `Range` relative to another `Node`.
		@throws DOMError
	**/
	function setEndAfter( refNode : Node ) : Void;
	
	/**
		Collapses the `Range` to one of its boundary points.
	**/
	function collapse( toStart : Bool = false ) : Void;
	
	/**
		Sets the `Range` to contain the `Node` and its contents.
		@throws DOMError
	**/
	function selectNode( refNode : Node ) : Void;
	
	/**
		Sets the `Range` to contain the contents of a `Node`.
		@throws DOMError
	**/
	function selectNodeContents( refNode : Node ) : Void;
	
	/**
		Compares the boundary points of the `Range` with another `Range`.
		@throws DOMError
	**/
	function compareBoundaryPoints( how : Int, sourceRange : Range ) : Int;
	
	/**
		Removes the contents of a `Range` from the `Document`.
		@throws DOMError
	**/
	function deleteContents() : Void;
	
	/**
		Moves contents of a `Range` from the document tree into a `DocumentFragment`.
		@throws DOMError
	**/
	function extractContents() : DocumentFragment;
	
	/**
		Returns a `DocumentFragment` copying the nodes of a `Range`.
		@throws DOMError
	**/
	function cloneContents() : DocumentFragment;
	
	/**
		Insert a `Node` at the start of a `Range`.
		@throws DOMError
	**/
	function insertNode( node : Node ) : Void;
	
	/**
		Moves content of a `Range` into a new `Node`.
		@throws DOMError
	**/
	function surroundContents( newParent : Node ) : Void;
	
	/**
		Returns a `Range` object with boundary points identical to the cloned `Range`.
	**/
	function cloneRange() : Range;
	
	/**
		Releases the `Range` from use to improve performance.
	**/
	function detach() : Void;
	
	/**
		Returns a `boolean` indicating whether the given point is in the `Range`.
		@throws DOMError
	**/
	function isPointInRange( node : Node, offset : Int ) : Bool;
	
	/**
		Returns -1, 0, or 1 indicating whether the point occurs before, inside, or after the `Range`.
		@throws DOMError
	**/
	function comparePoint( node : Node, offset : Int ) : Int;
	
	/**
		Returns a `boolean` indicating whether the given node intersects the `Range`.
		@throws DOMError
	**/
	function intersectsNode( node : Node ) : Bool;
	
	/**
		Returns a `DocumentFragment` created from a given string of code.
		@throws DOMError
	**/
	function createContextualFragment( fragment : String ) : DocumentFragment;
	
	/**
		Returns a list of `DOMRect` objects that aggregates the results of `Element.getClientRects()` for all the elements in the `Range`.
	**/
	function getClientRects() : DOMRectList;
	
	/**
		Returns a `DOMRect` object which bounds the entire contents of the `Range`; this would be the union of all the rectangles returned by `range.getClientRects()`.
	**/
	function getBoundingClientRect() : DOMRect;
}