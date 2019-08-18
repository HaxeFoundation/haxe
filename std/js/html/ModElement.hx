/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\HTMLModElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLModElement` interface provides special properties (beyond the regular methods and properties available through the `HTMLElement` interface they also have available to them by inheritance) for manipulating modification elements, that is `del` and `ins`.

	Documentation [HTMLModElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLModElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLModElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLModElement>
**/
@:native("HTMLModElement")
extern class ModElement extends Element {
	
	/**
		Is a `DOMString` reflecting the `cite` HTML attribute, containing a URI of a resource explaining the change.
	**/
	var cite : String;
	
	/**
		Is a `DOMString` reflecting the `datetime` HTML attribute, containing a date-and-time string representing a timestamp for the change.
	**/
	var dateTime : String;
	
}