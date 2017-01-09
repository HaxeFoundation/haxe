/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\SVGFEColorMatrixElement.webidl. Do not edit!

package js.html.svg;

@:native("SVGFEColorMatrixElement")
extern class FEColorMatrixElement extends Element
{
	static inline var SVG_FECOLORMATRIX_TYPE_UNKNOWN : Int = 0;
	static inline var SVG_FECOLORMATRIX_TYPE_MATRIX : Int = 1;
	static inline var SVG_FECOLORMATRIX_TYPE_SATURATE : Int = 2;
	static inline var SVG_FECOLORMATRIX_TYPE_HUEROTATE : Int = 3;
	static inline var SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA : Int = 4;
	
	var in1(default,null) : AnimatedString;
	var type(default,null) : AnimatedEnumeration;
	var values(default,null) : AnimatedNumberList;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}