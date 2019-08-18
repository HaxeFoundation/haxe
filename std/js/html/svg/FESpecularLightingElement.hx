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

// This file is generated from mozilla\SVGFESpecularLightingElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFESpecularLightingElement` interface corresponds to the `feSpecularLighting` element.

	Documentation [SVGFESpecularLightingElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpecularLightingElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpecularLightingElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpecularLightingElement>
**/
@:native("SVGFESpecularLightingElement")
extern class FESpecularLightingElement extends Element {
	
	/**
		An `SVGAnimatedString` corresponding to the `in` attribute of the given element.
	**/
	var in1(default,null) : AnimatedString;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `surfaceScale` attribute of the given element.
	**/
	var surfaceScale(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `specularConstant` attribute of the given element.
	**/
	var specularConstant(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `specularExponent` attribute of the given element.
	**/
	var specularExponent(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the X component of the `kernelUnitLength` attribute of the given element.
	**/
	var kernelUnitLengthX(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the Y component of the `kernelUnitLength` attribute of the given element.
	**/
	var kernelUnitLengthY(default,null) : AnimatedNumber;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}