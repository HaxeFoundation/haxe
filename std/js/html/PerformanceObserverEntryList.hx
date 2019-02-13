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

// This file is generated from mozilla\PerformanceObserverEntryList.webidl. Do not edit!

package js.html;

/**
	The `PerformanceObserverEntryList` interface is a list of peformance events that were explicitly observed via the `observe()` method.

	Documentation [PerformanceObserverEntryList](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserverEntryList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserverEntryList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserverEntryList>
**/
@:native("PerformanceObserverEntryList")
extern class PerformanceObserverEntryList {
	function getEntries( ?filter : PerformanceEntryFilterOptions ) : Array<PerformanceEntry>;
	function getEntriesByType( entryType : String ) : Array<PerformanceEntry>;
	function getEntriesByName( name : String, ?entryType : String ) : Array<PerformanceEntry>;
}