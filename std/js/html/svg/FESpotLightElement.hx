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

// This file is generated from mozilla\SVGFESpotLightElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGFESpotLightElement` interface corresponds to the `feSpotLight` element.

	Documentation [SVGFESpotLightElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpotLightElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpotLightElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpotLightElement>
**/
@:native("SVGFESpotLightElement")
extern class FESpotLightElement extends Element {
	
	/**
		An `SVGAnimatedNumber` corresponding to the `x` attribute of the given element.
	**/
	var x(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `y` attribute of the given element.
	**/
	var y(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `z` attribute of the given element.
	**/
	var z(default,null) : AnimatedNumber;
	var pointsAtX(default,null) : AnimatedNumber;
	var pointsAtY(default,null) : AnimatedNumber;
	var pointsAtZ(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `specularExponent` attribute of the given element.
	**/
	var specularExponent(default,null) : AnimatedNumber;
	
	/**
		An `SVGAnimatedNumber` corresponding to the `limitingConeAngle` attribute of the given element.
	**/
	var limitingConeAngle(default,null) : AnimatedNumber;
	
}