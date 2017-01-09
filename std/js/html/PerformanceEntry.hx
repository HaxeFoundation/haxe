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

// This file is generated from mozilla\PerformanceEntry.webidl. Do not edit!

package js.html;

/**
	The `PerformanceEntry` object encapsulates a single performance metric that is part of the performance timeline. A performance entry can be directly created by making a performance `mark` or `measure` (for example by calling the `mark()` method) at an explicit point in an application. Performance entries are also created in indirect ways such as loading a resource (such as an image).

	Documentation [PerformanceEntry](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceEntry) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceEntry$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceEntry>
**/
@:native("PerformanceEntry")
extern class PerformanceEntry
{
	
	/**
		A `DOMString` representing the name of a performance entry when the metric was created.
	**/
	var name(default,null) : String;
	
	/**
		A `DOMString` representing the type of performance metric such as "`mark`". See `PerformanceEntry.entryType` for a list of valid values.
	**/
	var entryType(default,null) : String;
	
	/**
		A `DOMHighResTimeStamp` representing the starting time for the performance metric.
	**/
	var startTime(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time value of the duration of the performance event.
	**/
	var duration(default,null) : Float;
	
}