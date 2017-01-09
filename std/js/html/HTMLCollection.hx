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

// This file is generated from mozilla\HTMLCollection.webidl. Do not edit!

package js.html;

/**
	The `HTMLCollection` interface represents a generic collection (array-like object similar to arguments) of elements (in document order) and offers methods and properties for selecting from the list.

	Documentation [HTMLCollection](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection>
**/
@:native("HTMLCollection")
extern class HTMLCollection implements ArrayAccess<Element>
{
	
	/**
		Returns the number of items in the collection.
	**/
	var length(default,null) : Int;
	
	
	/**
		Returns the specific node at the given zero-based `index` into the list. Returns `null` if the `index` is out of range.
	**/
	function item( index : Int ) : Element;
	
	/**
		Returns the specific node whose ID or, as a fallback, name matches the string specified by `name`. Matching by name is only done as a last resort, only in HTML, and only if the referenced element supports the `name` attribute. Returns `null` if no node exists by the given name.
	**/
	function namedItem( name : String ) : Element;
}