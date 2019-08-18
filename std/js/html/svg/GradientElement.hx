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

// This file is generated from mozilla\SVGGradientElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGGradient` interface is a base interface used by `SVGLinearGradientElement` and `SVGRadialGradientElement`.

	Documentation [SVGGradientElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGGradientElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGGradientElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGGradientElement>
**/
@:native("SVGGradientElement")
extern class GradientElement extends Element {
	static inline var SVG_SPREADMETHOD_UNKNOWN : Int = 0;
	static inline var SVG_SPREADMETHOD_PAD : Int = 1;
	static inline var SVG_SPREADMETHOD_REFLECT : Int = 2;
	static inline var SVG_SPREADMETHOD_REPEAT : Int = 3;
	
	
	/**
		Returns an `SVGAnimatedEnumeration` corresponding to the `gradientUnits` attribute on the given element. Takes one of the constants defined in `SVGUnitTypes`.
	**/
	var gradientUnits(default,null) : AnimatedEnumeration;
	
	/**
		Returns an `SVGAnimatedTransformList` corresponding to attribute `gradientTransform` on the given element.
	**/
	var gradientTransform(default,null) : AnimatedTransformList;
	
	/**
		Returns an `SVGAnimatedEnumeration` corresponding to attribute `spreadMethod` on the given element. One of the spread method types defined on this interface.
	**/
	var spreadMethod(default,null) : AnimatedEnumeration;
	var href(default,null) : AnimatedString;
	
}