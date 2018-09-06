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

// This file is generated from mozilla\PerformanceTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceTiming` interface is a legacy interface kept for backwards compatibility and contains properties that offer performance timing information for various events which occur during the loading and use of the current page. You get a `PerformanceTiming` object describing your page using the `window.performance.timing` property.

	Documentation [PerformanceTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming>
**/
@:native("PerformanceTiming")
extern class PerformanceTiming
{
	var navigationStart(default,null) : Int;
	var unloadEventStart(default,null) : Int;
	var unloadEventEnd(default,null) : Int;
	var redirectStart(default,null) : Int;
	var redirectEnd(default,null) : Int;
	var fetchStart(default,null) : Int;
	var domainLookupStart(default,null) : Int;
	var domainLookupEnd(default,null) : Int;
	var connectStart(default,null) : Int;
	var connectEnd(default,null) : Int;
	var secureConnectionStart(default,null) : Int;
	var requestStart(default,null) : Int;
	var responseStart(default,null) : Int;
	var responseEnd(default,null) : Int;
	var domLoading(default,null) : Int;
	var domInteractive(default,null) : Int;
	var domContentLoadedEventStart(default,null) : Int;
	var domContentLoadedEventEnd(default,null) : Int;
	var domComplete(default,null) : Int;
	var loadEventStart(default,null) : Int;
	var loadEventEnd(default,null) : Int;
	
	function toJSON() : Dynamic;
}