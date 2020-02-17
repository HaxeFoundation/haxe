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

// This file is generated from mozilla\SVGRect.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGRect` represents a rectangle. Rectangles consist of an x and y coordinate pair identifying a minimum x value, a minimum y value, and a width and height, which are constrained to be non-negative.

	Documentation [SVGRect](https://developer.mozilla.org/en-US/docs/Web/API/SVGRect) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGRect$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGRect>
**/
@:native("SVGRect")
extern class Rect {
	
	/**
		The exact effect of this coordinate depends on each element. If the attribute is not specified, the effect is as if a value of `0` were specified.
	**/
	var x : Float;
	
	/**
		The exact effect of this coordinate depends on each element.If the attribute is not specified, the effect is as if a value of `0` were specified.
	**/
	var y : Float;
	
	/**
		This represents the width of the rectangle.A value that is negative results to an error. A value of zero disables rendering of the element
	**/
	var width : Float;
	var height : Float;
	
}