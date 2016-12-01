/*
 * Copyright (C)2005-2016 Haxe Foundation
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

@:native("Range")
extern class Range
{
	static inline var START_TO_START : Int = 0;
	static inline var START_TO_END : Int = 1;
	static inline var END_TO_END : Int = 2;
	static inline var END_TO_START : Int = 3;
	
	var startContainer(default,null) : Node;
	var startOffset(default,null) : Int;
	var endContainer(default,null) : Node;
	var endOffset(default,null) : Int;
	var collapsed(default,null) : Bool;
	var commonAncestorContainer(default,null) : Node;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	function setStart( refNode : Node, offset : Int ) : Void;
	/** @throws DOMError */
	function setEnd( refNode : Node, offset : Int ) : Void;
	/** @throws DOMError */
	function setStartBefore( refNode : Node ) : Void;
	/** @throws DOMError */
	function setStartAfter( refNode : Node ) : Void;
	/** @throws DOMError */
	function setEndBefore( refNode : Node ) : Void;
	/** @throws DOMError */
	function setEndAfter( refNode : Node ) : Void;
	function collapse( ?toStart : Bool = false ) : Void;
	/** @throws DOMError */
	function selectNode( refNode : Node ) : Void;
	/** @throws DOMError */
	function selectNodeContents( refNode : Node ) : Void;
	/** @throws DOMError */
	function compareBoundaryPoints( how : Int, sourceRange : Range ) : Int;
	/** @throws DOMError */
	function deleteContents() : Void;
	/** @throws DOMError */
	function extractContents() : DocumentFragment;
	/** @throws DOMError */
	function cloneContents() : DocumentFragment;
	/** @throws DOMError */
	function insertNode( node : Node ) : Void;
	/** @throws DOMError */
	function surroundContents( newParent : Node ) : Void;
	function cloneRange() : Range;
	function detach() : Void;
	/** @throws DOMError */
	function isPointInRange( node : Node, offset : Int ) : Bool;
	/** @throws DOMError */
	function comparePoint( node : Node, offset : Int ) : Int;
	/** @throws DOMError */
	function intersectsNode( node : Node ) : Bool;
	/** @throws DOMError */
	function createContextualFragment( fragment : String ) : DocumentFragment;
	function getClientRects() : DOMRectList;
	function getBoundingClientRect() : DOMRect;
}