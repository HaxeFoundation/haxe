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

// This file is generated from mozilla\SVGElement.webidl. Do not edit!

package js.html.svg;

/**
	All of the SVG DOM interfaces that correspond directly to elements in the SVG language derive from the `SVGElement` interface.

	Documentation [SVGElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGElement>
**/
@:native("SVGElement")
extern class Element extends js.html.DOMElement
{
	
	/**
		An `SVGSVGElement` referring to the nearest ancestor `svg` element. `null` if the given element is the outermost `svg` element.
	**/
	var ownerSVGElement(default,null) : SVGElement;
	
	/**
		The `SVGElement`, which established the current viewport. Often, the nearest ancestor `svg` element. `null` if the given element is the outermost `svg` element.
	**/
	var viewportElement(default,null) : Element;
	
	/** @throws DOMError */
	function focus() : Void;
	/** @throws DOMError */
	function blur() : Void;
}