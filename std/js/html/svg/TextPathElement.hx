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

// This file is generated from mozilla\SVGTextPathElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGTextPathElement` interface corresponds to the `textPath` element.

	Documentation [SVGTextPathElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPathElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPathElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPathElement>
**/
@:native("SVGTextPathElement")
extern class TextPathElement extends TextContentElement {
	static inline var TEXTPATH_METHODTYPE_UNKNOWN : Int = 0;
	static inline var TEXTPATH_METHODTYPE_ALIGN : Int = 1;
	static inline var TEXTPATH_METHODTYPE_STRETCH : Int = 2;
	static inline var TEXTPATH_SPACINGTYPE_UNKNOWN : Int = 0;
	static inline var TEXTPATH_SPACINGTYPE_AUTO : Int = 1;
	static inline var TEXTPATH_SPACINGTYPE_EXACT : Int = 2;
	
	
	/**
		An `SVGAnimatedLength` corresponding to the X component of the `startOffset` attribute of the given element.
	**/
	var startOffset(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `method` attribute of the given element. It takes one of the `TEXTPATH_METHODTYPE_*` constants defined on this interface.
	**/
	var method(default,null) : AnimatedEnumeration;
	
	/**
		An `SVGAnimatedEnumeration` corresponding to the `spacing` attribute of the given element. It takes one of the `TEXTPATH_SPACINGTYPE_*` constants defined on this interface.
	**/
	var spacing(default,null) : AnimatedEnumeration;
	var href(default,null) : AnimatedString;
	
}