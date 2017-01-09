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

// This file is generated from mozilla\HTMLOListElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLOListElement` interface provides special properties (beyond those defined on the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating ordered list elements.

	Documentation [HTMLOListElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLOListElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLOListElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOListElement>
**/
@:native("HTMLOListElement")
extern class OListElement extends Element
{
	
	/**
		Is a `Boolean` value reflecting the `reversed` and defining if the numbering is descending, that is its value is `true`, or ascending (`false`).
	**/
	var reversed : Bool;
	
	/**
		Is a `long` value reflecting the `start` and defining the value of the first number of the first element of the list.
	**/
	var start : Int;
	
	/**
		Is a `DOMString` value reflecting the `type` and defining the kind of marker to be used to display. It can have the following values:
		 
		  `'1'` meaning that decimal numbers are used: `1`, `2`, `3`, `4`, `5`, …
		  `'a'` meaning that the lowercase latin alphabet is used:  `a`, `b`, `c`, `d`, `e`, …
		  `'A'` meaning that the uppercase latin alphabet is used: `A`, `B`, `C`, `D`, `E`, …
		  `'i'` meaning that the lowercase latin numerals are used: `i`, `ii`, `iii`, `iv`, `v`, …
		  `'I'` meaning that the uppercase latin numerals are used: `I`, `II`, `III`, `IV`, `V`, …
		 
		 
	**/
	var type : String;
	
	/**
		Is a `Boolean` indicating that spacing between list items should be reduced. This property reflects the `compact` attribute only, it doesn't consider the `line-height` CSS property used for that behavior in modern pages.
	**/
	var compact : Bool;
	
}