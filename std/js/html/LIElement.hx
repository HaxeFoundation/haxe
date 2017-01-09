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

// This file is generated from mozilla\HTMLLIElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLLIElement` interface expose specific properties and methods (beyond those defined by regular `HTMLElement` interface it also has available to it by inheritance) for manipulating list elements.

	Documentation [HTMLLIElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLIElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLIElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLIElement>
**/
@:native("HTMLLIElement")
extern class LIElement extends Element
{
	
	/**
		Is a `long` indicating the ordinal position of the list element inside a given `ol`. It reflects the `value` attribute of the HTML `li` element, and can be smaller than `0`. If the `li` element is not a child of an `ol` element, the property has no meaning.
	**/
	var value : Int;
	
	/**
		Is a `DOMString` representing the type of the bullets, `"disc"`, `"square"` or `"circle"`. As the standard way of defining the list type is via the CSS `list-style-type` property, use the CSSOM methods to set it via a script.
	**/
	var type : String;
	
}