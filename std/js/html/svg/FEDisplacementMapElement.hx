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

// This file is generated from mozilla\SVGFEDisplacementMapElement.webidl. Do not edit!

package js.html.svg;

@:native("SVGFEDisplacementMapElement")
extern class FEDisplacementMapElement extends Element
{
	static inline var SVG_CHANNEL_UNKNOWN : Int = 0;
	static inline var SVG_CHANNEL_R : Int = 1;
	static inline var SVG_CHANNEL_G : Int = 2;
	static inline var SVG_CHANNEL_B : Int = 3;
	static inline var SVG_CHANNEL_A : Int = 4;
	
	var in1(default,null) : AnimatedString;
	var in2(default,null) : AnimatedString;
	var scale(default,null) : AnimatedNumber;
	var xChannelSelector(default,null) : AnimatedEnumeration;
	var yChannelSelector(default,null) : AnimatedEnumeration;
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var result(default,null) : AnimatedString;
	
}