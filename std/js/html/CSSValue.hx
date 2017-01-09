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

// This file is generated from mozilla\CSSValue.webidl. Do not edit!

package js.html;

/**
	The `CSSValue` interface represents the current computed value of a CSS property.

	Documentation [CSSValue](https://developer.mozilla.org/en-US/docs/Web/API/CSSValue) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSValue$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSValue>
**/
@:native("CSSValue")
extern class CSSValue
{
	static inline var CSS_INHERIT : Int = 0;
	static inline var CSS_PRIMITIVE_VALUE : Int = 1;
	static inline var CSS_VALUE_LIST : Int = 2;
	static inline var CSS_CUSTOM : Int = 3;
	
	
	/**
		A `DOMString` representing the current value.
	**/
	var cssText : String;
	
	/**
		An <code>unsigned short</code> representing a code defining the type of the value. Possible values are:
		 <table class="standard-table">
		  
		   <tr>
		    <td class="header">Constant</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code>CSS_CUSTOM</code></td>
		    <td>The value is a custom value.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_INHERIT</code></td>
		    <td>The value is inherited and the <code>cssText</code> contains <code>"inherit"</code>.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_PRIMITIVE_VALUE</code></td>
		    <td>The value is a primitive value and an instance of the <code>CSSPrimitiveValue</code> interface can be obtained by using binding-specific casting methods on this instance of the <code>CSSValue</code> interface.</td>
		   </tr>
		   <tr>
		    <td><code>CSS_VALUE_LIST</code></td>
		    <td>The value is a <code>CSSValue</code> list and an instance of the <code>CSSValueList</code> interface can be obtained by using binding-specific casting methods on this instance of the <code>CSSValue</code> interface.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var cssValueType(default,null) : Int;
	
}