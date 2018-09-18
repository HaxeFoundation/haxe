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

// This file is generated from mozilla\SVGStyleElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGStyleElement` interface corresponds to the SVG `style` element.

	Documentation [SVGStyleElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGStyleElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGStyleElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGStyleElement>
**/
@:native("SVGStyleElement")
extern class StyleElement extends Element
{
	var xmlspace : String;
	
	/**
		A `DOMString` corresponding to the `type` attribute of the given element.
		 SVG 1.1 defined that a `DOMException` is raised with code `NO_MODIFICATION_ALLOWED_ERR` on an attempt to change the value of a read-only attribute. This restriction was removed in SVG 2.
		 
	**/
	var type : String;
	
	/**
		A `DOMString` corresponding to the `media` attribute of the given element.
		 SVG 1.1 defined that a `DOMException` is raised with code `NO_MODIFICATION_ALLOWED_ERR` on an attempt to change the value of a read-only attribute. This restriction was removed in SVG 2.
		 
	**/
	var media : String;
	var sheet(default,null) : js.html.StyleSheet;
	
}