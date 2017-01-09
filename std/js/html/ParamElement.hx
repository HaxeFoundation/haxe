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

// This file is generated from mozilla\HTMLParamElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLParamElement` interface provides special properties (beyond those of the regular `HTMLElement` object interface it inherits) for manipulating `param` elements, representing a pair of a key and a value that acts as a parameter for an `object` element.

	Documentation [HTMLParamElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLParamElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLParamElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLParamElement>
**/
@:native("HTMLParamElement")
extern class ParamElement extends Element
{
	
	/**
		Is a `DOMString` representing the name of the parameter. It reflects the `name` attribute.
	**/
	var name : String;
	
	/**
		Is a `DOMString` representing the value associated to the parameter. It reflects the `value` attribute.
	**/
	var value : String;
	
	/**
		Is a `DOMString` containing the type of the parameter when `valueType` has the `"ref"` value. It reflects the `type` attribute.
	**/
	var type : String;
	
	/**
		Is a `DOMString` containing the type of the `value`. It reflects the ``valuetype`` attribute and has one of the values: `"data"`, `"ref"`, or `"object"`.
	**/
	var valueType : String;
	
}