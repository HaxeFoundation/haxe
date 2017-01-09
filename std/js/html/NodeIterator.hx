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

// This file is generated from mozilla\NodeIterator.webidl. Do not edit!

package js.html;

/**
	The `NodeIterator` interface represents an iterator over the members of a list of the nodes in a subtree of the DOM. The nodes will be returned in document order.

	Documentation [NodeIterator](https://developer.mozilla.org/en-US/docs/Web/API/NodeIterator) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/NodeIterator$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/NodeIterator>
**/
@:native("NodeIterator")
extern class NodeIterator
{
	
	/**
		Returns a `Node` representing the root node as specified when the `NodeIterator` was created.
	**/
	var root(default,null) : Node;
	
	/**
		Returns the `Node` to which the iterator is anchored.
	**/
	var referenceNode(default,null) : Node;
	
	/**
		Returns a `Boolean` flag that indicates whether the `NodeIterator` is anchored before, the flag being `true`, or after, the flag being `false`, the anchor node.
	**/
	var pointerBeforeReferenceNode(default,null) : Bool;
	
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
		    <td><code>NodeFilter.SHOW_ATTRIBUTE</code> <em>(deprecated)</em></td>
		    <td><code>2</code></td>
		    <td>Shows attribute <code>Attr</code> nodes. This is meaningful only when creating a <code>NodeIterator</code> with an <code>Attr</code> node as its root; in this case, it means that the attribute node will appear in the first position of the iteration or traversal. Since attributes are never children of other nodes, they do not appear when traversing over the document tree.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_CDATA_SECTION</code> <em>(deprecated)</em></td>
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
		    <td><code>NodeFilter.SHOW_ENTITY</code> <em>(deprecated)</em></td>
		    <td><code>32</code></td>
		    <td>Shows <code>Entity</code> nodes. This is meaningful only when creating a <code>NodeIterator</code> with an <code>Entity</code> node as its root; in this case, it means that the <code>Entity</code> node will appear in the first position of the traversal. Since entities are not part of the document tree, they do not appear when traversing over the document tree.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_ENTITY_REFERENCE</code> <em>(deprecated)</em></td>
		    <td><code>16</code></td>
		    <td>Shows <code>EntityReference</code> nodes.</td>
		   </tr>
		   <tr>
		    <td><code>NodeFilter.SHOW_NOTATION</code> <em>(deprecated)</em></td>
		    <td><code>2048</code></td>
		    <td>Shows <code>Notation</code> nodes. This is meaningful only when creating a <code>NodeIterator</code> with a <code>Notation</code> node as its root; in this case, it means that the <code>Notation</code> node will appear in the first position of the traversal. Since entities are not part of the document tree, they do not appear when traversing over the document tree.</td>
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
	
	/** @throws DOMError */
	
	/**
		Returns the next `Node` in the document, or `null` if there are none.
	**/
	function nextNode() : Node;
	/** @throws DOMError */
	
	/**
		Returns the previous `Node` in the document, or `null` if there are none.
	**/
	function previousNode() : Node;
	
	/**
		This operation is a no-op. It doesn't do anything. Previously it was telling the engine that the `NodeIterator` was no more used, but this is now useless.
	**/
	function detach() : Void;
}