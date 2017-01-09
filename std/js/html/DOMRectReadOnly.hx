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

// This file is generated from mozilla\DOMRect.webidl. Do not edit!

package js.html;

/**
	The `DOMRectReadOnly` interface specifies the standard properties used by `DOMRect` to define a rectangle.

	Documentation [DOMRectReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/DOMRectReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMRectReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMRectReadOnly>
**/
@:native("DOMRectReadOnly")
extern class DOMRectReadOnly
{
	
	/**
		The x coordinate of the `DOMRect`'s origin.
	**/
	var x(default,null) : Float;
	
	/**
		The y coordinate of the `DOMRect`'s origin.
	**/
	var y(default,null) : Float;
	
	/**
		The width of the `DOMRect`.
	**/
	var width(default,null) : Float;
	
	/**
		The height of the `DOMRect`.
	**/
	var height(default,null) : Float;
	
	/**
		Returns the top coordinate value of the `DOMRect` (usually the same as `y`.)
	**/
	var top(default,null) : Float;
	
	/**
		Returns the right coordinate value of the `DOMRect` (usually the same as `x + width`).
	**/
	var right(default,null) : Float;
	
	/**
		Returns the bottom coordinate value of the `DOMRect` (usually the same as y + height).
	**/
	var bottom(default,null) : Float;
	
	/**
		Returns the left coordinate value of the `DOMRect` (usually the same as `x`).
	**/
	var left(default,null) : Float;
	
}