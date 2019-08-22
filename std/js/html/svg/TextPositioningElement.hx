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

// This file is generated from mozilla\SVGTextPositioningElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGTextPositioningElement` interface is implemented by elements that support attributes that position individual text glyphs. It is inherited by `SVGTextElement`, `SVGTSpanElement`, `SVGTRefElement` and `SVGAltGlyphElement`.

	Documentation [SVGTextPositioningElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPositioningElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPositioningElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPositioningElement>
**/
@:native("SVGTextPositioningElement")
extern class TextPositioningElement extends TextContentElement {
	
	/**
		Returns an `SVGAnimatedLengthList` reflecting the `x` attribute of the given element.
	**/
	var x(default,null) : AnimatedLengthList;
	
	/**
		Returns an `SVGAnimatedLengthList` reflecting the `y` attribute of the given element.
	**/
	var y(default,null) : AnimatedLengthList;
	
	/**
		Returns an `SVGAnimatedLengthList` reflecting the `dx` attribute of the given element.
	**/
	var dx(default,null) : AnimatedLengthList;
	
	/**
		Returns an `SVGAnimatedLengthList` reflecting the `dy` attribute of the given element.
	**/
	var dy(default,null) : AnimatedLengthList;
	
	/**
		Returns an `SVGAnimatedNumberList` reflecting the `rotate` attribute of the given element.
	**/
	var rotate(default,null) : AnimatedNumberList;
	
}