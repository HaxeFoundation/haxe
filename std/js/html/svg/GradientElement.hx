/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\SVGGradientElement.webidl line 15:0. Do not edit!

package js.html.svg;

@:native("SVGGradientElement")
extern class GradientElement extends Element
{
	static inline var SVG_SPREADMETHOD_UNKNOWN : Int = 0;
	static inline var SVG_SPREADMETHOD_PAD : Int = 1;
	static inline var SVG_SPREADMETHOD_REFLECT : Int = 2;
	static inline var SVG_SPREADMETHOD_REPEAT : Int = 3;
	static inline var SVG_UNIT_TYPE_UNKNOWN : Int = 0;
	static inline var SVG_UNIT_TYPE_USERSPACEONUSE : Int = 1;
	static inline var SVG_UNIT_TYPE_OBJECTBOUNDINGBOX : Int = 2;
	
	var gradientUnits(default,null) : AnimatedEnumeration;
	var gradientTransform(default,null) : AnimatedTransformList;
	var spreadMethod(default,null) : AnimatedEnumeration;
	var href(default,null) : AnimatedString;
	
}