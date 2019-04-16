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

// This file is generated from mozilla\SVGComponentTransferFunctionElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGComponentTransferFunctionElement` interface defines a base interface used by the component transfer function interfaces.

	Documentation [SVGComponentTransferFunctionElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGComponentTransferFunctionElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGComponentTransferFunctionElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGComponentTransferFunctionElement>
**/
@:native("SVGComponentTransferFunctionElement")
extern class ComponentTransferFunctionElement extends Element {
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN : Int = 0;
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY : Int = 1;
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_TABLE : Int = 2;
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE : Int = 3;
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_LINEAR : Int = 4;
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_GAMMA : Int = 5;
	
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `type` attribute of the given element. It takes one of the `SVG_FECOMPONENTTRANSFER_TYPE_*` constants defined on this interface.
	**/
	var type(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedNumberList` corresponding to the `tableValues` attribute of the given element.
	**/
	var tableValues(default,null) : AnimatedNumberList;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `slope` attribute of the given element.
	**/
	var slope(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `intercept` attribute of the given element.
	**/
	var intercept(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `amplitude` attribute of the given element.
	**/
	var amplitude(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `exponent` attribute of the given element.
	**/
	var exponent(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `offset` attribute of the given element.
	**/
	var offset(default,null) : AnimatedNumber;
	
}