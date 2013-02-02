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

/** <p>The <code>TreeWalker</code> object represents the nodes of a document subtree and a position within them.</p>
<p>A TreeWalker can be created using the <code><a title="en/DOM/document.createTreeWalker" rel="internal" href="https://developer.mozilla.org/en/DOM/document.createTreeWalker">createTreeWalker()</a></code> method of the <code><a title="en/DOM/document" rel="internal" href="https://developer.mozilla.org/en/DOM/document">document</a></code> object.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/Treewalker">MDN</a>. */
@:native("TreeWalker")
extern class TreeWalker
{
	/** Setter throws DOMException. */
	var currentNode : Node;

	var expandEntityReferences (default,null) : Bool;

	var filter (default,null) : NodeFilter;

	var root (default,null) : Node;

	var whatToShow (default,null) : Int;

	function firstChild() : Node;

	function lastChild() : Node;

	function nextNode() : Node;

	function nextSibling() : Node;

	function parentNode() : Node;

	function previousNode() : Node;

	function previousSibling() : Node;

}
