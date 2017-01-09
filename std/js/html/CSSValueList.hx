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

// This file is generated from mozilla\CSSValueList.webidl. Do not edit!

package js.html;

/**
	The `CSSValueList` interface derives from the `CSSValue` interface and provides the abstraction of an ordered collection of CSS values.

	Documentation [CSSValueList](https://developer.mozilla.org/en-US/docs/Web/API/CSSValueList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CSSValueList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CSSValueList>
**/
@:native("CSSValueList")
extern class CSSValueList extends CSSValue implements ArrayAccess<CSSValue>
{
	
	/**
		An `unsigned long` representing the number of `CSSValues` in the list.
	**/
	var length(default,null) : Int;
	
	
	/**
		This method is used to retrieve a `CSSValue` by ordinal index. The order in this collection represents the order of the values in the CSS style property. If index is greater than or equal to the number of values in the list, this returns `null`.
	**/
	function item( index : Int ) : CSSValue;
}