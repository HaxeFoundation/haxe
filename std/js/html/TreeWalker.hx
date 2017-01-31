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

// This file is generated from mozilla\TreeWalker.webidl. Do not edit!

package js.html;

/**
	The `TreeWalker` object represents the nodes of a document subtree and a position within them.

	Documentation [TreeWalker](https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker>
**/
@:native("TreeWalker")
extern class TreeWalker
{
	
	/**
		Returns a `Node` representing the root node as specified when the `TreeWalker` was created.
	**/
	var root(default,null) : Node;
	
	/**
		Returns an <code>unsigned long</code> being a bitmask made of constants describing the types of <code>Node</code> that must to be presented. Non-matching nodes are skipped, but their children may be included, if relevant. The possible values are:
		 <table class="standard-table">
		  
		   <tr>
		    <td class="header">Constant</td>
		    <td class="header">Numerical value</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ALL</code></td>
		    <td><code>-1</code> (that is the max value of <code>unsigned long</code>)</td>
		    <td>Shows all nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ATTRIBUTE</code> {{deprecated_inline}}</td>
		    <td><code>2</code></td>
		    <td>Shows attribute <code>Attr</code> nodes. This is meaningful only when creating a <code>TreeWalker</code> with an <code>Attr</code> node as its root; in this case, it means that the attribute node will appear in the first position of the iteration or traversal. Since attributes are never children of other nodes, they do not appear when traversing over the document tree.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_CDATA_SECTION</code> {{deprecated_inline}}</td>
		    <td><code>8</code></td>
		    <td>Shows <code>CDATASection</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_COMMENT</code></td>
		    <td><code>128</code></td>
		    <td>Shows <code>Comment</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_DOCUMENT</code></td>
		    <td><code>256</code></td>
		    <td>Shows <code>Document</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_DOCUMENT_FRAGMENT</code></td>
		    <td><code>1024</code></td>
		    <td>Shows <code>DocumentFragment</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_DOCUMENT_TYPE</code></td>
		    <td><code>512</code></td>
		    <td>Shows <code>DocumentType</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ELEMENT</code></td>
		    <td><code>1</code></td>
		    <td>Shows <code>Element</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ENTITY</code> {{deprecated_inline}}</td>
		    <td><code>32</code></td>
		    <td>Shows <code>Entity</code> nodes. This is meaningful only when creating a <code>TreeWalker</code> with an <code>Entity</code> node as its root; in this case, it means that the <code>Entity</code> node will appear in the first position of the traversal. Since entities are not part of the document tree, they do not appear when traversing over the document tree.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ENTITY_REFERENCE</code> {{deprecated_inline}}</td>
		    <td><code>16</code></td>
		    <td>Shows <code>EntityReference</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_NOTATION</code> {{deprecated_inline}}</td>
		    <td><code>2048</code></td>
		    <td>Shows <code>Notation</code> nodes. This is meaningful only when creating a <code>TreeWalker</code> with a <code>Notation</code> node as its root; in this case, it means that the <code>Notation</code> node will appear in the first position of the traversal. Since entities are not part of the document tree, they do not appear when traversing over the document tree.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_PROCESSING_INSTRUCTION</code></td>
		    <td><code>64</code></td>
		    <td>Shows <code>ProcessingInstruction</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_TEXT</code></td>
		    <td><code>4</code></td>
		    <td>Shows <code>Text</code> nodes.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var whatToShow(default,null) : Int;
	
	/**
		Returns a `NodeFilter` used to select the relevant nodes.
	**/
	var filter(default,null) : NodeFilter;
	
	/**
		Is the `Node` on which the `TreeWalker` is currently pointing at.
	**/
	var currentNode : Node;
	
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to the first visible ancestor node in the document order, and returns the found node. It also moves the current node to this one. If no such node exists, or if it is before that the root node defined at the object construction, returns `null` and the current node is not changed.
	**/
	function parentNode() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to the first visible child of the current node, and returns the found child. It also moves the current node to this child. If no such child exists, returns `null` and the current node is not changed.
	**/
	function firstChild() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to the last visible child of the current node, and returns the found child. It also moves the current node to this child. If no such child exists, returns `null` and the current node is not changed.
	**/
	function lastChild() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to its previous sibling, if any, and returns the found sibling. If there is no such node, return `null` and the current node is not changed.
	**/
	function previousSibling() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to its next sibling, if any, and returns the found sibling. If there is no such node, return `null` and the current node is not changed.
	**/
	function nextSibling() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to the previous visible node in the document order, and returns the found node. It also moves the current node to this one. If no such node exists,or if it is before that the root node defined at the object construction, returns `null` and the current node is not changed.
	**/
	function previousNode() : Node;
	/** @throws DOMError */
	
	/**
		Moves the current `Node` to the next visible node in the document order, and returns the found node. It also moves the current node to this one. If no such node exists, returns `null` and the current node is not changed.
	**/
	function nextNode() : Node;
}