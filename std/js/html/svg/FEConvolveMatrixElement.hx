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

// This file is generated from mozilla\SVGFEConvolveMatrixElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFEConvolveMatrixElement` interface corresponds to the `feConvolveMatrix` element.

	Documentation [SVGFEConvolveMatrixElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEConvolveMatrixElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEConvolveMatrixElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEConvolveMatrixElement>
**/
@:native("SVGFEConvolveMatrixElement")
extern class FEConvolveMatrixElement extends Element {
	static inline var SVG_EDGEMODE_UNKNOWN : Int = 0;
	static inline var SVG_EDGEMODE_DUPLICATE : Int = 1;
	static inline var SVG_EDGEMODE_WRAP : Int = 2;
	static inline var SVG_EDGEMODE_NONE : Int = 3;
	
	
	/**
		An `SVGAnimatedString` corresponding to the `in` attribute of the given element.
	**/
	var in1(default,null) : AnimatedString;
	
	/**
		An `SVGAnimatedInteger` corresponding to the `order` attribute of the given element.
	**/
	var orderX(default,null) : AnimatedInteger;
	
	/**
		An `SVGAnimatedInteger` corresponding to the `order` attribute of the given element.
	**/
	var orderY(default,null) : AnimatedInteger;
	
	/**
		An `SVGAnimatedNumberList` corresponding to the `kernelMatrix` attribute of the given element.
	**/
	var kernelMatrix(default,null) : AnimatedNumberList;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `divisor` attribute of the given element.
	**/
	var divisor(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `bias` attribute of the given element.
	**/
	var bias(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedInteger` corresponding to the `targetX` attribute of the given element.
	**/
	var targetX(default,null) : AnimatedInteger;
	
	/**
		An `SVGAnimatedInteger` corresponding to the `targetY` attribute of the given element.
	**/
	var targetY(default,null) : AnimatedInteger;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `edgeMode` attribute of the given element. Takes one of the `SVG_EDGEMODE_*` constants defined on this interface.
	**/
	var edgeMode(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `kernelUnitLength` attribute of the given element.
	**/
	var kernelUnitLengthX(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `kernelUnitLength` attribute of the given element.
	**/
	var kernelUnitLengthY(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedBoolean` corresponding to the `preserveAlpha` attribute of the given element.
	**/
	var preserveAlpha(default,null) : AnimatedBoolean;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}