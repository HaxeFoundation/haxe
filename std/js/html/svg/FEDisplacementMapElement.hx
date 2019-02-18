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

// This file is generated from mozilla\SVGFEDisplacementMapElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFEDisplacementMapElement` interface corresponds to the `feDisplacementMap` element.

	Documentation [SVGFEDisplacementMapElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDisplacementMapElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDisplacementMapElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDisplacementMapElement>
**/
@:native("SVGFEDisplacementMapElement")
extern class FEDisplacementMapElement extends Element {
	static inline var SVG_CHANNEL_UNKNOWN : Int = 0;
	static inline var SVG_CHANNEL_R : Int = 1;
	static inline var SVG_CHANNEL_G : Int = 2;
	static inline var SVG_CHANNEL_B : Int = 3;
	static inline var SVG_CHANNEL_A : Int = 4;
	
	
	/**
		An `SVGAnimatedString` corresponding to the `in` attribute of the given element.
	**/
	var in1(default,null) : AnimatedString;
	
	/**
		An `SVGAnimatedString` corresponding to the `in2` attribute of the given element.
	**/
	var in2(default,null) : AnimatedString;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `scale` attribute of the given element.
	**/
	var scale(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `xChannelSelect` attribute of the given element. It takes one of the `SVG_CHANNEL_*` constants defined on this interface.
	**/
	var xChannelSelector(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `yChannelSelect` attribute of the given element. It takes one of the `SVG_CHANNEL_*` constants defined on this interface.
	**/
	var yChannelSelector(default,null) : AnimatedEnumeration;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}