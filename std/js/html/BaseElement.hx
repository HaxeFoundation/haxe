/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\HTMLBaseElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLBaseElement` interface contains the base URI for a document. This object inherits all of the properties and methods as described in the `HTMLElement` interface.

	Documentation [HTMLBaseElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLBaseElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLBaseElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLBaseElement>
**/
@:native("HTMLBaseElement")
extern class BaseElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `href` HTML attribute, containing a base URL for relative URLs in the document.
	**/
	var href : String;
	
	/**
		Is a `DOMString` that reflects the `target` HTML attribute, containing a default target browsing context or frame for elements that do not have a target reference specified.
	**/
	var target : String;
	
}