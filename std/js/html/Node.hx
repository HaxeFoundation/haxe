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

// This file is generated from mozilla\Node.webidl. Do not edit!

package js.html;

/**
	A `Node` is an interface from which a number of DOM types inherit, and allows these various types to be treated (or tested) similarly.

	Documentation [Node](https://developer.mozilla.org/en-US/docs/Web/API/Node) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Node$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Node>
**/
@:native("Node")
extern class Node extends EventTarget
{
	static inline var ELEMENT_NODE : Int = 1;
	static inline var ATTRIBUTE_NODE : Int = 2;
	static inline var TEXT_NODE : Int = 3;
	static inline var CDATA_SECTION_NODE : Int = 4;
	static inline var ENTITY_REFERENCE_NODE : Int = 5;
	static inline var ENTITY_NODE : Int = 6;
	static inline var PROCESSING_INSTRUCTION_NODE : Int = 7;
	static inline var COMMENT_NODE : Int = 8;
	static inline var DOCUMENT_NODE : Int = 9;
	static inline var DOCUMENT_TYPE_NODE : Int = 10;
	static inline var DOCUMENT_FRAGMENT_NODE : Int = 11;
	static inline var NOTATION_NODE : Int = 12;
	static inline var DOCUMENT_POSITION_DISCONNECTED : Int = 1;
	static inline var DOCUMENT_POSITION_PRECEDING : Int = 2;
	static inline var DOCUMENT_POSITION_FOLLOWING : Int = 4;
	static inline var DOCUMENT_POSITION_CONTAINS : Int = 8;
	static inline var DOCUMENT_POSITION_CONTAINED_BY : Int = 16;
	static inline var DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC : Int = 32;
	
	
	/**
		Returns an <code>unsigned short</code> representing the type of the node. Possible values are:
		 <table class="standard-table">
		  
		   <tr>
		    Name
		    Value
		   </tr>
		   <tr>
		    <td><code>ELEMENT_NODE</code></td>
		    <td><code>1</code></td>
		   </tr>
		   <tr>
		    <td><code>ATTRIBUTE_NODE</code> <em>(deprecated)</em></td>
		    <td><code>2</code></td>
		   </tr>
		   <tr>
		    <td><code>TEXT_NODE</code></td>
		    <td><code>3</code></td>
		   </tr>
		   <tr>
		    <td><code>CDATA_SECTION_NODE</code> <em>(deprecated)</em></td>
		    <td><code>4</code></td>
		   </tr>
		   <tr>
		    <td><code>ENTITY_REFERENCE_NODE</code> <em>(deprecated)</em></td>
		    <td><code>5</code></td>
		   </tr>
		   <tr>
		    <td><code>ENTITY_NODE</code> <em>(deprecated)</em></td>
		    <td><code>6</code></td>
		   </tr>
		   <tr>
		    <td><code>PROCESSING_INSTRUCTION_NODE</code></td>
		    <td><code>7</code></td>
		   </tr>
		   <tr>
		    <td><code>COMMENT_NODE</code></td>
		    <td><code>8</code></td>
		   </tr>
		   <tr>
		    <td><code>DOCUMENT_NODE</code></td>
		    <td><code>9</code></td>
		   </tr>
		   <tr>
		    <td><code>DOCUMENT_TYPE_NODE</code></td>
		    <td><code>10</code></td>
		   </tr>
		   <tr>
		    <td><code>DOCUMENT_FRAGMENT_NODE</code></td>
		    <td><code>11</code></td>
		   </tr>
		   <tr>
		    <td><code>NOTATION_NODE</code> <em>(deprecated)</em></td>
		    <td><code>12</code></td>
		   </tr>
		  
		 </table>
		 
	**/
	var nodeType(default,null) : Int;
	
	/**
		Returns a `DOMString` containing the name of the `Node`. The structure of the name will differ with the name type. E.g. An `HTMLElement` will contain the name of the corresponding tag, like `'audio'` for an `HTMLAudioElement`, a `Text` node will have the `'#text'` string, or a `Document` node will have the `'#document'` string.
	**/
	var nodeName(default,null) : String;
	
	/**
		Returns a `DOMString` representing the base URL. The concept of base URL changes from one language to another; in HTML, it corresponds to the protocol, the domain name and the directory structure, that is all until the last `'/'`.
	**/
	var baseURI(default,null) : String;
	
	/**
		Returns the `Document` that this node belongs to. If no document is associated with it, returns `null`.
	**/
	var ownerDocument(default,null) : HTMLDocument;
	
	/**
		Returns a `Node` that is the parent of this node. If there is no such node, like if this node is the top of the tree or if doesn't participate in a tree, this property returns `null`.
	**/
	var parentNode(default,null) : Node;
	
	/**
		Returns an `Element` that is the parent of this node. If the node has no parent, or if that parent is not an `Element`, this property returns `null`.
	**/
	var parentElement(default,null) : Element;
	
	/**
		Returns a live `NodeList` containing all the children of this node. `NodeList` being live means that if the children of the `Node` change, the `NodeList` object is automatically updated.
	**/
	var childNodes(default,null) : NodeList;
	
	/**
		Returns a `Node` representing the first direct child node of the node, or `null` if the node has no child.
	**/
	var firstChild(default,null) : Node;
	
	/**
		Returns a `Node` representing the last direct child node of the node, or `null` if the node has no child.
	**/
	var lastChild(default,null) : Node;
	
	/**
		Returns a `Node` representing the previous node in the tree, or `null` if there isn't such node.
	**/
	var previousSibling(default,null) : Node;
	
	/**
		Returns a `Node` representing the next node in the tree, or `null` if there isn't such node.
	**/
	var nextSibling(default,null) : Node;
	
	/**
		Returns / Sets the value of the current node
	**/
	var nodeValue : String;
	
	/**
		Returns / Sets the textual content of an element and all its descendants.
	**/
	var textContent : String;
	
	/**
		The namespace URI of this node, or `null` if it is no namespace.
		 
		 Note: In Firefox 3.5 and earlier, HTML elements are in no namespace. In later versions, HTML elements are in the `https://www.w3.org/1999/xhtml/` namespace in both HTML and XML trees. `1.9.2`
		 
		 
	**/
	var namespaceURI(default,null) : String;
	
	/**
		Is a `DOMString` representing the namespace prefix of the node, or `null` if no prefix is specified.
	**/
	var prefix(default,null) : String;
	
	/**
		Returns a `DOMString` representing the local part of the qualified name of an element.
		 
		 Note: In Firefox 3.5 and earlier, the property upper-cases the local name for HTML elements (but not XHTML elements). In later versions, this does not happen, so the property is in lower case for both HTML and XHTML. `1.9.2`
		 
		 
	**/
	var localName(default,null) : String;
	
	
	/**
		Returns a `Boolean` indicating if the element has any child nodes, or not.
	**/
	function hasChildNodes() : Bool;
	/** @throws DOMError */
	
	/**
		Inserts the first `Node` given in a parameter immediately before the second, child of this element, `Node`.
	**/
	function insertBefore( node : Node, child : Node ) : Node;
	/** @throws DOMError */
	
	/**
		Adds the specified childNode argument as the last child to the current node.
		
		 If the argument referenced an existing node on the DOM tree, the node will be detached from its current position and attached at the new position.
	**/
	function appendChild( node : Node ) : Node;
	/** @throws DOMError */
	
	/**
		Replaces one child `Node` of the current one with the second one given in parameter.
	**/
	function replaceChild( node : Node, child : Node ) : Node;
	/** @throws DOMError */
	
	/**
		Removes a child node from the current element, which must be a child of the current node.
	**/
	function removeChild( child : Node ) : Node;
	
	/**
		Clean up all the text nodes under this element (merge adjacent, remove empty).
	**/
	function normalize() : Void;
	/** @throws DOMError */
	
	/**
		Clone a `Node`, and optionally, all of its contents. By default, it clones the content of the node.
	**/
	function cloneNode( ?deep : Bool = false ) : Node;
	
	/**
		Returns a `Boolean` which indicates whether or not two nodes are of the same type and all their defining data points match.
	**/
	function isEqualNode( node : Node ) : Bool;
	
	/**
		Returns the context objects root which optionally includes the shadow root if it is available. 
	**/
	function compareDocumentPosition( other : Node ) : Int;
	
	/**
		Returns the context objects root which optionally includes the shadow root if it is available. 
	**/
	function contains( other : Node ) : Bool;
	
	/**
		Clean up all the text nodes under this element (merge adjacent, remove empty).
	**/
	function lookupPrefix( namespace_ : String ) : String;
	
	/**
		Clean up all the text nodes under this element (merge adjacent, remove empty).
	**/
	function lookupNamespaceURI( prefix : String ) : String;
	
	/**
		Returns a `Boolean` which indicates whether or not two nodes are of the same type and all their defining data points match.
	**/
	function isDefaultNamespace( namespace_ : String ) : Bool;
}