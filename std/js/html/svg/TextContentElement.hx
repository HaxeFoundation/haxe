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

// This file is generated from mozilla\SVGTextContentElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGTextContentElement` interface is implemented by elements that support rendering child text content. It is inherited by various text-related interfaces, such as `SVGTextElement`, `SVGTSpanElement`, `SVGTRefElement`, `SVGAltGlyphElement` and `SVGTextPathElement`.

	Documentation [SVGTextContentElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextContentElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGTextContentElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextContentElement>
**/
@:native("SVGTextContentElement")
extern class TextContentElement extends GraphicsElement {
	static inline var LENGTHADJUST_UNKNOWN : Int = 0;
	static inline var LENGTHADJUST_SPACING : Int = 1;
	static inline var LENGTHADJUST_SPACINGANDGLYPHS : Int = 2;
	
	
	/**
		An `SVGAnimatedLength` reflecting the `textLength` attribute of the given element.
	**/
	var textLength(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedEnumeration` reflecting the `lengthAdjust` attribute of the given element. The numeric type values represent one of the constant values above.
	**/
	var lengthAdjust(default,null) : AnimatedEnumeration;
	
	
	/**
		Returns a long representing the total number of addressable characters available for rendering within the current element, regardless of whether they will be rendered.
	**/
	function getNumberOfChars() : Int;
	
	/**
		Returns a float representing the computed length for the text within the element.
	**/
	function getComputedTextLength() : Float;
	
	/**
		Returns a float representing the computed length of the formatted text advance distance for a substring of text within the element. Note that this method only accounts for the widths of the glyphs in the substring and any extra spacing inserted by the CSS 'letter-spacing' and 'word-spacing' properties. Visual spacing adjustments made by the 'x' attribute is ignored.
		@throws DOMError
	**/
	function getSubStringLength( charnum : Int, nchars : Int ) : Float;
	
	/**
		Returns a `DOMPoint` representing the position of a typographic character after text layout has been performed.
		 Note: In SVG 1.1 this method returned an `SVGPoint`.
		 
		@throws DOMError
	**/
	function getStartPositionOfChar( charnum : Int ) : Point;
	
	/**
		Returns a `DOMPoint` representing the trailing position of a typographic character after text layout has been performed.
		 Note: In SVG 1.1 this method returned an `SVGPoint`.
		 
		@throws DOMError
	**/
	function getEndPositionOfChar( charnum : Int ) : Point;
	
	/**
		Returns a `DOMRect` representing the computed tight bounding box of the glyph cell that corresponds to a given typographic character.
		@throws DOMError
	**/
	function getExtentOfChar( charnum : Int ) : Rect;
	
	/**
		Returns a float representing the rotation of typographic character.
		@throws DOMError
	**/
	function getRotationOfChar( charnum : Int ) : Float;
	
	/**
		Returns a long representing the character which caused a text glyph to be rendered at a given position in the coordinate system. Because the relationship between characters and glyphs is not one-to-one, only the first character of the relevant typographic character is returned
	**/
	function getCharNumAtPosition( point : Point ) : Int;
	
	/**
		Selects text within the element.
		@throws DOMError
	**/
	function selectSubString( charnum : Int, nchars : Int ) : Void;
}