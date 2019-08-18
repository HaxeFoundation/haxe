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

// This file is generated from mozilla\Performance.webidl. Do not edit!

package js.html;

/**
	The `Performance` interface provides access to performance-related information for the current page. It's part of the High Resolution Time API, but is enhanced by the Performance Timeline API, the Navigation Timing API, the User Timing API, and the Resource Timing API.

	Documentation [Performance](https://developer.mozilla.org/en-US/docs/Web/API/Performance) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Performance$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Performance>
**/
@:native("Performance")
extern class Performance extends EventTarget {
	
	/**
		Returns the high resolution timestamp of the start time of the performance measurement.
	**/
	var timeOrigin(default,null) : Float;
	
	/**
		A `PerformanceTiming` object containing latency-related performance information
	**/
	var timing(default,null) : PerformanceTiming;
	
	/**
		A `PerformanceNavigation` object that provides useful context about the operations included in the times listed in `timing`, including whether the page was a load or a refresh, how many redirections occurred, and so forth.
	**/
	var navigation(default,null) : PerformanceNavigation;
	
	/**
		An `EventTarget` which is a callback that will be called when the `resourcetimingbufferfull` event is fired.
	**/
	var onresourcetimingbufferfull : haxe.Constraints.Function;
	
	
	/**
		Returns a `DOMHighResTimeStamp` representing the number of milliseconds elapsed since a reference instant.
	**/
	function now() : Float;
	
	/**
		Is a jsonizer returning a json object representing the `Performance` object.
	**/
	function toJSON() : Dynamic;
	
	/**
		Returns a list of `PerformanceEntry` objects based on the given filter.
	**/
	function getEntries() : Array<PerformanceEntry>;
	
	/**
		Returns a list of `PerformanceEntry` objects of the given entry type.
	**/
	function getEntriesByType( entryType : String ) : Array<PerformanceEntry>;
	
	/**
		Returns a list of `PerformanceEntry` objects based on the given name and entry type.
	**/
	function getEntriesByName( name : String, ?entryType : String ) : Array<PerformanceEntry>;
	
	/**
		Removes all `PerformanceEntry` with a `PerformanceEntry.entryType` of "`resource`" from the browser's performance data buffer.
	**/
	function clearResourceTimings() : Void;
	
	/**
		Sets the browser's resource timing buffer size to the specified number of "`resource`" `PerformanceEntry.entryType` `PerformanceEntry` objects.
	**/
	function setResourceTimingBufferSize( maxSize : Int ) : Void;
	
	/**
		Creates a `DOMHighResTimeStamp` in the browser's performance entry buffer with the given name.
		@throws DOMError
	**/
	function mark( markName : String ) : Void;
	
	/**
		Removes the given mark from the browser's performance entry buffer.
	**/
	function clearMarks( ?markName : String ) : Void;
	
	/**
		Creates a named `DOMHighResTimeStamp` in the browser's performance entry buffer between two specified marks (known as the start mark and end mark, respectively).
		@throws DOMError
	**/
	function measure( measureName : String, ?startMark : String, ?endMark : String ) : Void;
	
	/**
		Removes the given measure from the browser's performance entry buffer.
	**/
	function clearMeasures( ?measureName : String ) : Void;
}