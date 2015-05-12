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

// This file is generated from mozilla/Node.webidl line 16:0. Do not edit!

package js.html;

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
	
	var nodeType(default,null) : Int;
	var nodeName(default,null) : String;
	var baseURI(default,null) : String;
	var ownerDocument(default,null) : HTMLDocument;
	var parentNode(default,null) : Node;
	var parentElement(default,null) : Element;
	var childNodes(default,null) : NodeList;
	var firstChild(default,null) : Node;
	var lastChild(default,null) : Node;
	var previousSibling(default,null) : Node;
	var nextSibling(default,null) : Node;
	var nodeValue : String;
	var textContent : String;
	var namespaceURI(default,null) : String;
	var prefix(default,null) : String;
	var localName(default,null) : String;
	
	function hasChildNodes() : Bool;
	/** @throws DOMError */
	function insertBefore( node : Node, child : Node ) : Node;
	/** @throws DOMError */
	function appendChild( node : Node ) : Node;
	/** @throws DOMError */
	function replaceChild( node : Node, child : Node ) : Node;
	/** @throws DOMError */
	function removeChild( child : Node ) : Node;
	function normalize() : Void;
	/** @throws DOMError */
	function cloneNode( ?deep : Bool = false ) : Node;
	function isEqualNode( node : Node ) : Bool;
	function compareDocumentPosition( other : Node ) : Int;
	function contains( other : Node ) : Bool;
	function lookupPrefix( namespace_ : String ) : String;
	function lookupNamespaceURI( prefix : String ) : String;
	function isDefaultNamespace( namespace_ : String ) : Bool;
}