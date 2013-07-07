/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** Obsolete<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/font">MDN</a>. */
@:native("HTMLFontElement")
extern class FontElement extends Element
{
	/** This attribute sets the text color using either a named color or a color specified in the hexadecimal #RRGGBB format. */
	var color : String;

	/** This attribute contains a comma-sperated list of one or more font names. The document text in the default style is rendered in the first font face that the client's browser supports. If no font listed is installed on the local system, the browser typically defaults to the proportional or fixed-width font for that system. */
	var face : String;

	/** This attribute specifies the font size as either a numeric or relative value. Numeric values range from <span>1</span> to <span>7</span> with <span>1</span> being the smallest and <span>3</span> the default. It can be defined using a relative value, like <span>+2</span> or <span>-3</span>, which set it relative to the value of the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/basefont#attr-size">size</a></code>
 attribute of the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/basefont">&lt;basefont&gt;</a></code>
 element, or relative to <span>3</span>, the default value, if none does exist. */
	var size : String;

}
