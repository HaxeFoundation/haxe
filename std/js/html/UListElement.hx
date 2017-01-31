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

// This file is generated from mozilla\HTMLUListElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLUListElement` interface provides special properties (beyond those defined on the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating unordered list elements.

	Documentation [HTMLUListElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLUListElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLUListElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLUListElement>
**/
@:native("HTMLUListElement")
extern class UListElement extends Element
{
	
	/**
		Is a `Boolean` indicating that spacing between list items should be reduced. This property reflects the `compact` attribute only, it doesn't consider the `line-height` CSS property used for that behavior in modern pages.
	**/
	var compact : Bool;
	
	/**
		Is a `DOMString` value reflecting the `type` and defining the kind of marker to be used to display. The values are browser dependent and have never been standardized.
	**/
	var type : String;
	
}