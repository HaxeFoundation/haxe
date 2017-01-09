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

// This file is generated from mozilla\TouchList.webidl. Do not edit!

package js.html;

/**
	The `TouchList` interface represents a list of contact points with a touch surface; for example, if the user has three fingers on the touch surface (such as a screen or trackpad), the corresponding `TouchList` object would have one `Touch` object for each finger, for a total of three entries.

	Documentation [TouchList](https://developer.mozilla.org/en-US/docs/Web/API/TouchList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TouchList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TouchList>
**/
@:native("TouchList")
extern class TouchList implements ArrayAccess<Touch>
{
	
	/**
		The number of `Touch` objects in the `TouchList`.
	**/
	var length(default,null) : Int;
	
	
	/**
		Returns the `Touch` object at the specified index in the list.
	**/
	function item( index : Int ) : Touch;
	
	/**
		Returns the first `Touch` item in the list whose identifier matches a specified value.
	**/
	function identifiedTouch( identifier : Int ) : Touch;
}