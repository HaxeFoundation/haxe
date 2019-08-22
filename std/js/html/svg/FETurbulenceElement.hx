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

// This file is generated from mozilla\SVGFETurbulenceElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFETurbulenceElement` interface corresponds to the `feTurbulence` element.

	Documentation [SVGFETurbulenceElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFETurbulenceElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFETurbulenceElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETurbulenceElement>
**/
@:native("SVGFETurbulenceElement")
extern class FETurbulenceElement extends Element {
	static inline var SVG_TURBULENCE_TYPE_UNKNOWN : Int = 0;
	static inline var SVG_TURBULENCE_TYPE_FRACTALNOISE : Int = 1;
	static inline var SVG_TURBULENCE_TYPE_TURBULENCE : Int = 2;
	static inline var SVG_STITCHTYPE_UNKNOWN : Int = 0;
	static inline var SVG_STITCHTYPE_STITCH : Int = 1;
	static inline var SVG_STITCHTYPE_NOSTITCH : Int = 2;
	
	
	/**
		An `SVGAnimatedNumber` corresponding to the X component of the `baseFrequency` attribute of the given element.
	**/
	var baseFrequencyX(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the Y component of the `baseFrequency` attribute of the given element.
	**/
	var baseFrequencyY(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedInteger` corresponding to the `numOctaves` attribute of the given element.
	**/
	var numOctaves(default,null) : AnimatedInteger;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `seed` attribute of the given element.
	**/
	var seed(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `stitchTiles` attribute of the given element. It takes one of the `SVG_STITCHTYPE_*` constants defined on this interface.
	**/
	var stitchTiles(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `type` attribute of the given element. It takes one of the `SVG_TURBULENCE_TYPE_*` constants defined on this interface.
	**/
	var type(default,null) : AnimatedEnumeration;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}