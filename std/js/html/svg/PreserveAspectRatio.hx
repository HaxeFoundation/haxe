/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from mozilla\SVGPreserveAspectRatio.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGPreserveAspectRatio` interface corresponds to the `preserveAspectRatio` attribute, which is available for some of SVG's elements.

	Documentation [SVGPreserveAspectRatio](https://developer.mozilla.org/en-US/docs/Web/API/SVGPreserveAspectRatio) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGPreserveAspectRatio$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGPreserveAspectRatio>
**/
@:native("SVGPreserveAspectRatio")
extern class PreserveAspectRatio
{
	static inline var SVG_PRESERVEASPECTRATIO_UNKNOWN : Int = 0;
	static inline var SVG_PRESERVEASPECTRATIO_NONE : Int = 1;
	static inline var SVG_PRESERVEASPECTRATIO_XMINYMIN : Int = 2;
	static inline var SVG_PRESERVEASPECTRATIO_XMIDYMIN : Int = 3;
	static inline var SVG_PRESERVEASPECTRATIO_XMAXYMIN : Int = 4;
	static inline var SVG_PRESERVEASPECTRATIO_XMINYMID : Int = 5;
	static inline var SVG_PRESERVEASPECTRATIO_XMIDYMID : Int = 6;
	static inline var SVG_PRESERVEASPECTRATIO_XMAXYMID : Int = 7;
	static inline var SVG_PRESERVEASPECTRATIO_XMINYMAX : Int = 8;
	static inline var SVG_PRESERVEASPECTRATIO_XMIDYMAX : Int = 9;
	static inline var SVG_PRESERVEASPECTRATIO_XMAXYMAX : Int = 10;
	static inline var SVG_MEETORSLICE_UNKNOWN : Int = 0;
	static inline var SVG_MEETORSLICE_MEET : Int = 1;
	static inline var SVG_MEETORSLICE_SLICE : Int = 2;
	
	var align : Int;
	var meetOrSlice : Int;
	
}