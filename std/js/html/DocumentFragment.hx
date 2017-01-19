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

// This file is generated from mozilla\DocumentFragment.webidl. Do not edit!

package js.html;

/**
	The `DocumentFragment` interface represents a minimal document object that has no parent. It is used as a light-weight version of `Document` to store well-formed or potentially non-well-formed fragments of XML.

	Documentation [DocumentFragment](https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment>
**/
@:native("DocumentFragment")
extern class DocumentFragment extends Node
{
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
	/** @throws DOMError */
	
	/**
		Returns the first `Element` node within the `DocumentFragment`, in document order, that matches the specified selectors.
	**/
	function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	
	/**
		Returns a `NodeList` of all the `Element` nodes within the `DocumentFragment` that match the specified selectors.
	**/
	function querySelectorAll( selectors : String ) : NodeList;
}