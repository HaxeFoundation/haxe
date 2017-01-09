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

// This file is generated from mozilla\SVGLength.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGLength` interface correspond to the length basic data type.

	Documentation [SVGLength](https://developer.mozilla.org/en-US/docs/Web/API/SVGLength) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGLength$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGLength>
**/
@:native("SVGLength")
extern class Length
{
	static inline var SVG_LENGTHTYPE_UNKNOWN : Int = 0;
	static inline var SVG_LENGTHTYPE_NUMBER : Int = 1;
	static inline var SVG_LENGTHTYPE_PERCENTAGE : Int = 2;
	static inline var SVG_LENGTHTYPE_EMS : Int = 3;
	static inline var SVG_LENGTHTYPE_EXS : Int = 4;
	static inline var SVG_LENGTHTYPE_PX : Int = 5;
	static inline var SVG_LENGTHTYPE_CM : Int = 6;
	static inline var SVG_LENGTHTYPE_MM : Int = 7;
	static inline var SVG_LENGTHTYPE_IN : Int = 8;
	static inline var SVG_LENGTHTYPE_PT : Int = 9;
	static inline var SVG_LENGTHTYPE_PC : Int = 10;
	
	var unitType(default,null) : Int;
	var value : Float;
	var valueInSpecifiedUnits : Float;
	var valueAsString : String;
	
	/** @throws DOMError */
	function newValueSpecifiedUnits( unitType : Int, valueInSpecifiedUnits : Float ) : Void;
	/** @throws DOMError */
	function convertToSpecifiedUnits( unitType : Int ) : Void;
}