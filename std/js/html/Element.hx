/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from mozilla\HTMLElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLElement` interface represents any HTML element. Some elements directly implement this interface, others implement it via an interface that inherits it.

	Documentation [HTMLElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement>
**/
@:native("HTMLElement")
extern class Element extends DOMElement
{
	
	/**
		Sends a mouse click event to the element.
	**/
	function click() : Void;
	
	/**
		Makes the element the current keyboard focus.
		@throws DOMError
	**/
	function focus() : Void;
	
	/**
		Removes keyboard focus from the currently focused element.
		@throws DOMError
	**/
	function blur() : Void;
}