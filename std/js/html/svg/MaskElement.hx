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

// This file is generated from mozilla\SVGMaskElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGMaskElement` interface provides access to the properties of `mask` elements, as well as methods to manipulate them.

	Documentation [SVGMaskElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGMaskElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGMaskElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGMaskElement>
**/
@:native("SVGMaskElement")
extern class MaskElement extends Element {
	static inline var SVG_MASKTYPE_LUMINANCE : Int = 0;
	static inline var SVG_MASKTYPE_ALPHA : Int = 1;
	
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `maskUnits` attribute of the given `mask` element. Takes one of the constants defined in `SVGUnitTypes`.
	**/
	var maskUnits(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `maskContentUnits` attribute of the given `mask` element. Takes one of the constants defined in `SVGUnitTypes`.
	**/
	var maskContentUnits(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedLength` corresponding to the `x` attribute of the given `mask` element.
	**/
	var x(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `y` attribute of the given `mask` element.
	**/
	var y(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `width` attribute of the given `mask` element.
	**/
	var width(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `height` attribute of the given `mask` element.
	**/
	var height(default,null) : AnimatedLength;
	
}