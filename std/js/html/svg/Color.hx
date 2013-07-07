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
package js.html.svg;

/** <p>This page explains more about how you can specify color in CSS.
</p><p>In your sample stylesheet, you introduce background colors.
</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/it/Conoscere_i_CSS/Colori">MDN</a>. */
@:native("SVGColor")
extern class Color extends js.html.CSSValue
{
	static inline var SVG_COLORTYPE_CURRENTCOLOR : Int = 3;

	static inline var SVG_COLORTYPE_RGBCOLOR : Int = 1;

	static inline var SVG_COLORTYPE_RGBCOLOR_ICCCOLOR : Int = 2;

	static inline var SVG_COLORTYPE_UNKNOWN : Int = 0;

	var colorType(default,null) : Int;

	var rgbColor(default,null) : js.html.RGBColor;

	function setColor( colorType : Int, rgbColor : String, iccColor : String ) : Void;

	function setRGBColor( rgbColor : String ) : Void;

	function setRGBColorICCColor( rgbColor : String, iccColor : String ) : Void;

}
