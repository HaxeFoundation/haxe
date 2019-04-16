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

// This file is generated from mozilla\SVGFEMorphologyElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFEMorphologyElement` interface corresponds to the `feMorphology` element.

	Documentation [SVGFEMorphologyElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMorphologyElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMorphologyElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMorphologyElement>
**/
@:native("SVGFEMorphologyElement")
extern class FEMorphologyElement extends Element {
	static inline var SVG_MORPHOLOGY_OPERATOR_UNKNOWN : Int = 0;
	static inline var SVG_MORPHOLOGY_OPERATOR_ERODE : Int = 1;
	static inline var SVG_MORPHOLOGY_OPERATOR_DILATE : Int = 2;
	
	
	/**
		An `SVGAnimatedString` corresponding to the `in` attribute of the given element.
	**/
	var in1(default,null) : AnimatedString;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `operator` attribute of the given element. It takes one of the `SVG_MORPHOLOGY_OPERATOR_*` constants defined on this interface.
	**/
	@:native("operator")
	var operator_(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedNumber` corresponding to the X component of the `radius` attribute of the given element.
	**/
	var radiusX(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the Y component of the `radius` attribute of the given element.
	**/
	var radiusY(default,null) : AnimatedNumber;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}