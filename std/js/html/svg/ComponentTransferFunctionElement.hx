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

@:native("SVGComponentTransferFunctionElement")
extern class ComponentTransferFunctionElement extends Element
{
	static inline var SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE : Int = 3;

	static inline var SVG_FECOMPONENTTRANSFER_TYPE_GAMMA : Int = 5;

	static inline var SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY : Int = 1;

	static inline var SVG_FECOMPONENTTRANSFER_TYPE_LINEAR : Int = 4;

	static inline var SVG_FECOMPONENTTRANSFER_TYPE_TABLE : Int = 2;

	static inline var SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN : Int = 0;

	var amplitude(default,null) : AnimatedNumber;

	var exponent(default,null) : AnimatedNumber;

	var intercept(default,null) : AnimatedNumber;

	var offset(default,null) : AnimatedNumber;

	var slope(default,null) : AnimatedNumber;

	var tableValues(default,null) : AnimatedNumberList;

	var type(default,null) : AnimatedEnumeration;

}
