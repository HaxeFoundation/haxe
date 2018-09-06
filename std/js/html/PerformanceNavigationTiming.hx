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

// This file is generated from mozilla\PerformanceNavigationTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceNavigationTiming` interface provides methods and properties to store and retrieve metrics regarding the browser's document navigation events. For example, this interface can be used to determine how much time it takes to load or unload a document.

	Documentation [PerformanceNavigationTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming>
**/
@:native("PerformanceNavigationTiming")
extern class PerformanceNavigationTiming extends PerformanceResourceTiming
{
	var unloadEventStart(default,null) : Float;
	var unloadEventEnd(default,null) : Float;
	var domInteractive(default,null) : Float;
	var domContentLoadedEventStart(default,null) : Float;
	var domContentLoadedEventEnd(default,null) : Float;
	var domComplete(default,null) : Float;
	var loadEventStart(default,null) : Float;
	var loadEventEnd(default,null) : Float;
	var type(default,null) : NavigationType;
	var redirectCount(default,null) : Int;
	
	
	/**
		Returns a `DOMString` that is the JSON representation of the `PerformanceNavigationTiming` object.
	**/
	function toJSON() : Dynamic;
}