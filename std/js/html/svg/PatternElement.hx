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

// This file is generated from mozilla\SVGPatternElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGPatternElement` interface corresponds to the `pattern` element.

	Documentation [SVGPatternElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGPatternElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGPatternElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGPatternElement>
**/
@:native("SVGPatternElement")
extern class PatternElement extends Element {
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `patternUnits` attribute of the given `pattern` element. Takes one of the constants defined in `SVGUnitTypes`.
	**/
	var patternUnits(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `patternContentUnits` attribute of the given `pattern` element. Takes one of the constants defined in `SVGUnitTypes`.
	**/
	var patternContentUnits(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedTransformList` corresponding to the `patternTransform` attribute of the given `pattern` element.
	**/
	var patternTransform(default,null) : AnimatedTransformList;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `x` attribute of the given `pattern` element.
	**/
	var x(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `y` attribute of the given `pattern` element.
	**/
	var y(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `width` attribute of the given `pattern` element.
	**/
	var width(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `height` attribute of the given `pattern` element.
	**/
	var height(default,null) : AnimatedLength;
	var viewBox(default,null) : AnimatedRect;
	var preserveAspectRatio(default,null) : AnimatedPreserveAspectRatio;
	var href(default,null) : AnimatedString;
	
}