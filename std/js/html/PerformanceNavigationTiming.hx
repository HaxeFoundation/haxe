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

// This file is generated from mozilla\PerformanceNavigationTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceNavigationTiming` interface provides methods and properties to store and retrieve metrics regarding the browser's document navigation events. For example, this interface can be used to determine how much time it takes to load or unload a document.

	Documentation [PerformanceNavigationTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigationTiming>
**/
@:native("PerformanceNavigationTiming")
extern class PerformanceNavigationTiming extends PerformanceResourceTiming {
	
	/**
		A `DOMHighResTimeStamp` representing the time value equal to the time immediately before the user agent starts the unload event of the previous document.
	**/
	var unloadEventStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time value equal to the time immediately after the user agent finishes the unload event of the previous document.
	**/
	var unloadEventEnd(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing a `DOMHighResTimeStamp` representing the time value equal to the time immediately before the user agent sets the current document readiness of the current document to interactive.
	**/
	var domInteractive(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time value equal to the time immediately before the user agent fires the DOMContentLoaded event at the current document.
	**/
	var domContentLoadedEventStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time value equal to the time immediately after the current document's DOMContentLoaded event completes.
	**/
	var domContentLoadedEventEnd(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing a time value equal to the time immediately before the browser sets the current document readiness of the current document to complete.
	**/
	var domComplete(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time value equal to the time immediately before the load event of the current document is fired.
	**/
	var loadEventStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time when the load event of the current document is completed.
	**/
	var loadEventEnd(default,null) : Float;
	
	/**
		A `DOMString` representing the navigation type. Must be: "`navigate`", "`reload`", "`back_forward`" or "`prerender`".
	**/
	var type(default,null) : NavigationType;
	
	/**
		A number representing the number of redirects since the last non-redirect navigation under the current browsing context.
	**/
	var redirectCount(default,null) : Int;
	
	
	/**
		Returns a `DOMString` that is the JSON representation of the `PerformanceNavigationTiming` object.
	**/
	function toJSON() : Dynamic;
}