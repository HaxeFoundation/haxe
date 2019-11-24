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

// This file is generated from mozilla\DocumentFragment.webidl. Do not edit!

package js.html;

/**
	The `DocumentFragment` interface represents a minimal document object that has no parent. It is used as a lightweight version of `Document` that stores a segment of a document structure comprised of nodes just like a standard document. The key difference is that because the document fragment isn't part of the active document tree structure, changes made to the fragment don't affect the document, cause reflow, or incur any performance impact that can occur when changes are made.

	Documentation [DocumentFragment](https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment>
**/
@:native("DocumentFragment")
extern class DocumentFragment extends Node {
	var children(default,null) : HTMLCollection;
	var firstElementChild(default,null) : Element;
	var lastElementChild(default,null) : Element;
	var childElementCount(default,null) : Int;
	
	/** @throws DOMError */
	function new() : Void;
	
	/**
		Returns the first `Element` node within the DocumentFragment`, in document order, that matches the specified ID.
	**/
	function getElementById( elementId : String ) : Element;
	
	/**
		Returns the first `Element` node within the `DocumentFragment`, in document order, that matches the specified selectors.
		@throws DOMError
	**/
	function querySelector( selectors : String ) : Element;
	
	/**
		Returns a `NodeList` of all the `Element` nodes within the `DocumentFragment` that match the specified selectors.
		@throws DOMError
	**/
	function querySelectorAll( selectors : String ) : NodeList;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function prepend( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function append( nodes : haxe.extern.Rest<Node> ) : Void;
}