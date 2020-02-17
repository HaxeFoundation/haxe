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

// This file is generated from mozilla\HTMLFontElement.webidl. Do not edit!

package js.html;

/**
	Implements the document object model (DOM) representation of the font element. The HTML Font Element `font` defines the font size, font face and color of text.

	Documentation [HTMLFontElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFontElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFontElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFontElement>
**/
@:native("HTMLFontElement")
extern class FontElement extends Element {
	
	/**
		Is a `DOMString` that reflects the `color` HTML attribute, containing either a named color or a color specified in the hexadecimal #RRGGBB format.
	**/
	var color : String;
	
	/**
		Is a `DOMString` that reflects the `face` HTML attribute, containing a comma-separated list of one or more font names.
	**/
	var face : String;
	
	/**
		Is a `DOMString` that reflects the `size` HTML attribute, containing either a font size number ranging from 1 to 7 or a relative size to the `size` attribute of the `basefont` element, for example -2 or +1.
	**/
	var size : String;
	
}