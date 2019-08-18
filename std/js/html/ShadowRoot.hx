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

// This file is generated from mozilla\ShadowRoot.webidl. Do not edit!

package js.html;

/**
	The `ShadowRoot` interface of the Shadow DOM API is the root node of a DOM subtree that is rendered separately from a document's main DOM tree.

	Documentation [ShadowRoot](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot>
**/
@:native("ShadowRoot")
extern class ShadowRoot extends DocumentFragment {
	
	/**
		The mode of the `ShadowRoot` — either `open` or `closed`. This defines whether or not the shadow root's internal features are accessible from JavaScript.
	**/
	var mode(default,null) : ShadowRootMode;
	
	/**
		Returns a reference to the DOM element the `ShadowRoot` is attached to.
	**/
	var host(default,null) : Element;
	
	/**
		Sets or returns a reference to the DOM tree inside the `ShadowRoot`.
	**/
	var innerHTML : String;
	var activeElement(default,null) : Element;
	var styleSheets(default,null) : StyleSheetList;
	var pointerLockElement(default,null) : Element;
	var fullscreenElement(default,null) : Element;
	
	function getElementById( elementId : String ) : Element;
	function getElementsByTagName( localName : String ) : HTMLCollection;
	function getElementsByTagNameNS( namespace : String, localName : String ) : HTMLCollection;
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	function elementFromPoint( x : Float, y : Float ) : Element;
	function elementsFromPoint( x : Float, y : Float ) : Array<Element>;
}