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

// This file is generated from mozilla\SVGCircleElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGCircleElement` interface is an interface for the `circle` element. The circle element is defined by the cx and cy attributes that denote the coordinates of the centre of the circle.

	Documentation [SVGCircleElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGCircleElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGCircleElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGCircleElement>
**/
@:native("SVGCircleElement")
extern class CircleElement extends GeometryElement {
	
	/**
		
		 This property defines the x-coordinate of the center of the circle element. It is denoted by the `cx` attribute of the `circle` element. If unspecified, the value of this attribute is assumed to be `0`.
		
		 It can be animated by SVG's animation elements.
		 
	**/
	var cx(default,null) : AnimatedLength;
	
	/**
		
		 This property defines the y-coordinate of the center of the circle element. It is denoted by the `cy` attribute of the `circle` element. If unspecified, the value of this attribute is assumed to be `0`.
		
		 It can be animated by SVG's animation elements.
		 
	**/
	var cy(default,null) : AnimatedLength;
	
	/**
		
		 This property defines the radius of the circle element. It is denoted by the `r` of the `circle` element. A negative value gives an error, while `0` disables the rendering of the element.
		
		 It can be animated by SVG's animation elements.
		 
	**/
	var r(default,null) : AnimatedLength;
	
}