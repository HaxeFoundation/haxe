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

@:native("CSSPrimitiveValue")
extern class CSSPrimitiveValue extends CSSValue
{
	static inline var CSS_ATTR : Int = 22;

	static inline var CSS_CM : Int = 6;

	static inline var CSS_COUNTER : Int = 23;

	static inline var CSS_DEG : Int = 11;

	static inline var CSS_DIMENSION : Int = 18;

	static inline var CSS_EMS : Int = 3;

	static inline var CSS_EXS : Int = 4;

	static inline var CSS_GRAD : Int = 13;

	static inline var CSS_HZ : Int = 16;

	static inline var CSS_IDENT : Int = 21;

	static inline var CSS_IN : Int = 8;

	static inline var CSS_KHZ : Int = 17;

	static inline var CSS_MM : Int = 7;

	static inline var CSS_MS : Int = 14;

	static inline var CSS_NUMBER : Int = 1;

	static inline var CSS_PC : Int = 10;

	static inline var CSS_PERCENTAGE : Int = 2;

	static inline var CSS_PT : Int = 9;

	static inline var CSS_PX : Int = 5;

	static inline var CSS_RAD : Int = 12;

	static inline var CSS_RECT : Int = 24;

	static inline var CSS_RGBCOLOR : Int = 25;

	static inline var CSS_S : Int = 15;

	static inline var CSS_STRING : Int = 19;

	static inline var CSS_UNKNOWN : Int = 0;

	static inline var CSS_URI : Int = 20;

	static inline var CSS_VH : Int = 27;

	static inline var CSS_VMIN : Int = 28;

	static inline var CSS_VW : Int = 26;

	var primitiveType (default,null) : Int;

	function getCounterValue() : Counter;

	function getFloatValue( unitType : Int ) : Float;

	function getRGBColorValue() : RGBColor;

	function getRectValue() : Rect;

	function getStringValue() : String;

	function setFloatValue( unitType : Int, floatValue : Float ) : Void;

	function setStringValue( stringType : Int, stringValue : String ) : Void;

}
