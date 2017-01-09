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

// This file is generated from mozilla\TimeRanges.webidl. Do not edit!

package js.html;

/**
	The `TimeRanges` interface is used to represent a set of time ranges, primarily for the purpose of tracking which portions of media have been buffered when loading it for use by the `audio` and `video` elements.

	Documentation [TimeRanges](https://developer.mozilla.org/en-US/docs/Web/API/TimeRanges) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TimeRanges$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TimeRanges>
**/
@:native("TimeRanges")
extern class TimeRanges
{
	
	/**
		Returns an `unsigned long` representing the number of time ranges represented by the time range object.
	**/
	var length(default,null) : Int;
	
	/** @throws DOMError */
	
	/**
		Returns the time for the start of the range with the specified index.
	**/
	function start( index : Int ) : Float;
	/** @throws DOMError */
	
	/**
		Returns the time for the end of the specified range.
	**/
	function end( index : Int ) : Float;
}