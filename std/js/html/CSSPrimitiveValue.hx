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

// This file is generated from mozilla\CSSPrimitiveValue.webidl. Do not edit!

package js.html;

/**
	The `CSSPrimitiveValue` interface derives from the `CSSValue` interface and represents the current computed value of a CSS property.

	Documentation [CSSPrimitiveValue](https://developer.mozilla.org/en-US/docs/Web/API/CSSPrimitiveValue) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSPrimitiveValue$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSPrimitiveValue>
**/
@:native("CSSPrimitiveValue")
extern class CSSPrimitiveValue extends CSSValue
{
	static inline var CSS_UNKNOWN : Int = 0;
	static inline var CSS_NUMBER : Int = 1;
	static inline var CSS_PERCENTAGE : Int = 2;
	static inline var CSS_EMS : Int = 3;
	static inline var CSS_EXS : Int = 4;
	static inline var CSS_PX : Int = 5;
	static inline var CSS_CM : Int = 6;
	static inline var CSS_MM : Int = 7;
	static inline var CSS_IN : Int = 8;
	static inline var CSS_PT : Int = 9;
	static inline var CSS_PC : Int = 10;
	static inline var CSS_DEG : Int = 11;
	static inline var CSS_RAD : Int = 12;
	static inline var CSS_GRAD : Int = 13;
	static inline var CSS_MS : Int = 14;
	static inline var CSS_S : Int = 15;
	static inline var CSS_HZ : Int = 16;
	static inline var CSS_KHZ : Int = 17;
	static inline var CSS_DIMENSION : Int = 18;
	static inline var CSS_STRING : Int = 19;
	static inline var CSS_URI : Int = 20;
	static inline var CSS_IDENT : Int = 21;
	static inline var CSS_ATTR : Int = 22;
	static inline var CSS_COUNTER : Int = 23;
	static inline var CSS_RECT : Int = 24;
	static inline var CSS_RGBCOLOR : Int = 25;
	
	
	/**
		An <code>unsigned short</code> representing the type of the value. Possible values are:
		 <table class="standard-table">
		  
		   <tr>
		    <td class="header">Constant</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code>CSS_ATTR</code></td>
		    <td>The value is an <code>attr()</code> function. The value can be obtained by using the <code>getStringValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_CM</code></td>
		    <td>The value is a <code>length</code> in centimeters. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_COUNTER</code></td>
		    <td>The value is a counter or counters function. The value can be obtained by using the <code>getCounterValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_DEG</code></td>
		    <td>The value is an <code>angle</code> in degrees. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_DIMENSION</code></td>
		    <td>The value is a <code>number</code> with an unknown dimension. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_EMS</code></td>
		    <td>The value is a <code>length</code> in em units. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_EXS</code></td>
		    <td>The value is a <code>length</code> in ex units. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_GRAD</code></td>
		    <td>The value is an <code>angle</code> in grads. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_HZ</code></td>
		    <td>The value is a <code>frequency</code> in Hertz. The value can be obtained by using the getFloatValue method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_IDENT</code></td>
		    <td>The value is an identifier. The value can be obtained by using the <code>getStringValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_IN</code></td>
		    <td>The value is a <code>length</code> in inches. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_KHZ</code></td>
		    <td>The value is a <code>frequency</code> in Kilohertz. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_MM</code></td>
		    <td>The value is a <code>length</code> in millimeters. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_MS</code></td>
		    <td>The value is a <code>time</code> in milliseconds. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_NUMBER</code></td>
		    <td>The value is a simple <code>number</code>. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_PC</code></td>
		    <td>The value is a <code>length</code> in picas. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_PERCENTAGE</code></td>
		    <td>The value is a <code>percentage</code>. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_PT</code></td>
		    <td>The value is a <code>length</code> in points. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_PX</code></td>
		    <td>The value is a <code>length</code> in pixels. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_RAD</code></td>
		    <td>The value is an <code>angle</code> in radians. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_RECT</code></td>
		    <td>The value is a <code>shape</code> function. The value can be obtained by using the <code>getRectValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_RGBCOLOR</code></td>
		    <td>The value is an <code>color</code>. The value can be obtained by using the <code>getRGBColorValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_S</code></td>
		    <td>The value is a <code>time</code> in seconds. The value can be obtained by using the <code>getFloatValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_STRING</code></td>
		    <td>The value is a <code>string</code>. The value can be obtained by using the <code>getStringValue()</code> method.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_UNKNOWN</code></td>
		    <td>The value is not a recognized CSS2 value. The value can only be obtained by using the <code>CSSValue.cssText</code> attribute.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_URI</code></td>
		    <td>The value is a <code>uri</code>. The value can be obtained by using the <code>getStringValue()</code> method.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var primitiveType(default,null) : Int;
	
	/** @throws DOMError */
	
	/**
		A method to set the float value with a specified unit. If the property attached with this value can not accept the specified unit or the float value, the value will be unchanged and a `DOMException` will be raised.
	**/
	function setFloatValue( unitType : Int, floatValue : Float ) : Void;
	/** @throws DOMError */
	
	/**
		This method is used to get a float value in a specified unit. If this CSS value doesn't contain a float value or can't be converted into the specified unit, a `DOMException` is raised.
	**/
	function getFloatValue( unitType : Int ) : Float;
	/** @throws DOMError */
	
	/**
		A method to set the string value with the specified unit. If the property attached to this value can't accept the specified unit or the string value, the value will be unchanged and a `DOMException` will be raised.
	**/
	function setStringValue( stringType : Int, stringValue : String ) : Void;
	/** @throws DOMError */
	
	/**
		This method is used to get the string value. If the CSS value doesn't contain a string value, a `DOMException` is raised.
	**/
	function getStringValue() : String;
	/** @throws DOMError */
	
	/**
		This method is used to get the counter value. If this CSS value doesn't contain a counter value, a `DOMException` is raised. Modification to the corresponding style property can be achieved using the `Counter` interface.
	**/
	function getCounterValue() : Dynamic/*MISSING Counter*/;
	/** @throws DOMError */
	
	/**
		This method is used to get the Rect value. If this CSS value doesn't contain a rect value, a `DOMException` is raised. Modification to the corresponding style property can be achieved using the `Rect` interface.
	**/
	function getRectValue() : Rect;
	/** @throws DOMError */
	
	/**
		This method is used to get the RGB color. If this CSS value doesn't contain a RGB color value, a `DOMException` is raised. Modification to the corresponding style property can be achieved using the `RGBColor` interface.
	**/
	function getRGBColorValue() : RGBColor;
}