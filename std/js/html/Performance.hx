/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from mozilla\Performance.webidl. Do not edit!

package js.html;

/**
	The `Performance` interface represents timing-related performance information for the given page.

	Documentation [Performance](https://developer.mozilla.org/en-US/docs/Web/API/Performance) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Performance$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Performance>
**/
@:native("Performance")
extern class Performance
{
	
	/**
		Is a `PerformanceTiming` object containing latency-related performance information.
	**/
	var timing(default,null) : PerformanceTiming;
	
	/**
		Is a `PerformanceNavigation` object representing the type of navigation that occurs in the given browsing context, like the amount of redirections needed to fetch the resource.
	**/
	var navigation(default,null) : PerformanceNavigation;
	
	
	/**
		Returns a `DOMHighResTimeStamp` representing the amount of milliseconds elapsed since a reference instant.
	**/
	function now() : Float;
}