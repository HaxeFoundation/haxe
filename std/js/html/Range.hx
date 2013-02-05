/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** <p>The <code>Range</code> object represents a fragment of a document that can contain nodes and parts of text nodes in a given document.</p>
<p>A range can be created using the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Document.createRange">Document.createRange</a></code>
&nbsp;method of the&nbsp;<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Document">Document</a></code>
&nbsp;object. Range objects can also be retrieved by using the <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/Selection.getRangeAt" class="new">Selection.getRangeAt</a></code>
&nbsp;method of the&nbsp;<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Selection">Selection</a></code>
&nbsp;object.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/range">MDN</a>. */
@:native("Range")
extern class Range
{
	static inline var END_TO_END : Int = 2;

	static inline var END_TO_START : Int = 3;

	static inline var NODE_AFTER : Int = 1;

	static inline var NODE_BEFORE : Int = 0;

	static inline var NODE_BEFORE_AND_AFTER : Int = 2;

	static inline var NODE_INSIDE : Int = 3;

	static inline var START_TO_END : Int = 1;

	static inline var START_TO_START : Int = 0;

	/** Returns a&nbsp;<code>boolean</code>&nbsp;indicating whether the range's start and end points are at the same position. Getter throws DOMException. */
	var collapsed(default,null) : Bool;

	/** Returns the deepest&nbsp;<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>
&nbsp;that contains the startContainer and endContainer Nodes. Getter throws DOMException. */
	var commonAncestorContainer(default,null) : Node;

	/** Returns the&nbsp;<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>
&nbsp;within which the Range ends. Getter throws DOMException. */
	var endContainer(default,null) : Node;

	/** Returns a number representing where in the endContainer the Range ends. Getter throws DOMException. */
	var endOffset(default,null) : Int;

	/** Returns the&nbsp;<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>
&nbsp;within which the Range starts. Getter throws DOMException. */
	var startContainer(default,null) : Node;

	/** Returns a number representing where in the startContainer the Range starts. Getter throws DOMException. */
	var startOffset(default,null) : Int;

	function cloneContents() : DocumentFragment;

	function cloneRange() : Range;

	function collapse( toStart : Bool ) : Void;

	function compareBoundaryPoints( how : Int, sourceRange : Range ) : Int;

	function compareNode( refNode : Node ) : Int;

	function comparePoint( refNode : Node, offset : Int ) : Int;

	function createContextualFragment( html : String ) : DocumentFragment;

	function deleteContents() : Void;

	function detach() : Void;

	function expand( unit : String ) : Void;

	function extractContents() : DocumentFragment;

	function getBoundingClientRect() : ClientRect;

	function getClientRects() : ClientRectList;

	function insertNode( newNode : Node ) : Void;

	function intersectsNode( refNode : Node ) : Bool;

	function isPointInRange( refNode : Node, offset : Int ) : Bool;

	function selectNode( refNode : Node ) : Void;

	function selectNodeContents( refNode : Node ) : Void;

	function setEnd( refNode : Node, offset : Int ) : Void;

	function setEndAfter( refNode : Node ) : Void;

	function setEndBefore( refNode : Node ) : Void;

	function setStart( refNode : Node, offset : Int ) : Void;

	function setStartAfter( refNode : Node ) : Void;

	function setStartBefore( refNode : Node ) : Void;

	function surroundContents( newParent : Node ) : Void;

	function toString() : String;

}
