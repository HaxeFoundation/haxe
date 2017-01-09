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

// This file is generated from mozilla\SVGFEConvolveMatrixElement.webidl. Do not edit!

package js.html.svg;

@:native("SVGFEConvolveMatrixElement")
extern class FEConvolveMatrixElement extends Element
{
	static inline var SVG_EDGEMODE_UNKNOWN : Int = 0;
	static inline var SVG_EDGEMODE_DUPLICATE : Int = 1;
	static inline var SVG_EDGEMODE_WRAP : Int = 2;
	static inline var SVG_EDGEMODE_NONE : Int = 3;
	
	var in1(default,null) : AnimatedString;
	var orderX(default,null) : AnimatedInteger;
	var orderY(default,null) : AnimatedInteger;
	var kernelMatrix(default,null) : AnimatedNumberList;
	var divisor(default,null) : AnimatedNumber;
	var bias(default,null) : AnimatedNumber;
	var targetX(default,null) : AnimatedInteger;
	var targetY(default,null) : AnimatedInteger;
	var edgeMode(default,null) : AnimatedEnumeration;
	var kernelUnitLengthX(default,null) : AnimatedNumber;
	var kernelUnitLengthY(default,null) : AnimatedNumber;
	var preserveAlpha(default,null) : AnimatedBoolean;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}