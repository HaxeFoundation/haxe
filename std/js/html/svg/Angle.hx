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

// This file is generated from mozilla\SVGAngle.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGAngle` interface is used to represent a value that can be an `angle` or `number` value. An `SVGAngle` reflected through the `animVal` attribute is always read only.

	Documentation [SVGAngle](https://developer.mozilla.org/en-US/docs/Web/API/SVGAngle) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGAngle$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGAngle>
**/
@:native("SVGAngle")
extern class Angle
{
	static inline var SVG_ANGLETYPE_UNKNOWN : Int = 0;
	static inline var SVG_ANGLETYPE_UNSPECIFIED : Int = 1;
	static inline var SVG_ANGLETYPE_DEG : Int = 2;
	static inline var SVG_ANGLETYPE_RAD : Int = 3;
	static inline var SVG_ANGLETYPE_GRAD : Int = 4;
	
	
	/**
		The type of the value as specified by one of the `SVG_ANGLETYPE_*` constants defined on this interface.
	**/
	var unitType(default,null) : Int;
	
	/**
		
		 The value as a floating point value, in user units. Setting this attribute will cause `valueInSpecifiedUnits` and `valueAsString` to be updated automatically to reflect this setting.
		
		 Exceptions on setting: a `DOMException` with code `NO_MODIFICATION_ALLOWED_ERR` is raised when the length corresponds to a read only attribute or when the object itself is read only.
		 
	**/
	var value : Float;
	
	/**
		
		 The value as a floating point value, in the units expressed by `unitType`. Setting this attribute will cause `value` and `valueAsString` to be updated automatically to reflect this setting.
		
		 Exceptions on setting: a `DOMException` with code `NO_MODIFICATION_ALLOWED_ERR` is raised when the length corresponds to a read only attribute or when the object itself is read only.
		 
	**/
	var valueInSpecifiedUnits : Float;
	
	/**
		
		 The value as a `DOMString` value, in the units expressed by `unitType`. Setting this attribute will cause `value`, `valueInSpecifiedUnits` and `unitType` to be updated automatically to reflect this setting.
		
		 Exceptions on setting:
		 a `DOMException` with code `SYNTAX_ERR` is raised if the assigned string cannot be parsed as a valid `angle`. a `DOMException` with code `NO_MODIFICATION_ALLOWED_ERR` is raised when the length corresponds to a read only attribute or when the object itself is read only.
	**/
	var valueAsString : String;
	
	/** @throws DOMError */
	function newValueSpecifiedUnits( unitType : Int, valueInSpecifiedUnits : Float ) : Void;
	/** @throws DOMError */
	function convertToSpecifiedUnits( unitType : Int ) : Void;
}