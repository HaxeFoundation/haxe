/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/Selection.webidl line 13:0. Do not edit!

package js.html;

@:native("Selection")
extern class Selection
{
	var anchorNode(default,null) : Node;
	var anchorOffset(default,null) : Int;
	var focusNode(default,null) : Node;
	var focusOffset(default,null) : Int;
	var isCollapsed(default,null) : Bool;
	var rangeCount(default,null) : Int;
	
	/** @throws DOMError */
	function collapse( node : Node, offset : Int ) : Void;
	/** @throws DOMError */
	function collapseToStart() : Void;
	/** @throws DOMError */
	function collapseToEnd() : Void;
	/** @throws DOMError */
	function extend( node : Node, offset : Int ) : Void;
	/** @throws DOMError */
	function selectAllChildren( node : Node ) : Void;
	/** @throws DOMError */
	function deleteFromDocument() : Void;
	/** @throws DOMError */
	function getRangeAt( index : Int ) : Range;
	/** @throws DOMError */
	function addRange( range : Range ) : Void;
	/** @throws DOMError */
	function removeRange( range : Range ) : Void;
	/** @throws DOMError */
	function removeAllRanges() : Void;
	/** @throws DOMError */
	function containsNode( node : Node, allowPartialContainment : Bool ) : Bool;
	/** @throws DOMError */
	function modify( alter : String, direction : String, granularity : String ) : Void;
}