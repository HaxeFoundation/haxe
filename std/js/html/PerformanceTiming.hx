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

// This file is generated from mozilla\PerformanceTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceTiming` interface is a legacy interface kept for backwards compatibility and contains properties that offer performance timing information for various events which occur during the loading and use of the current page. You get a `PerformanceTiming` object describing your page using the `window.performance.timing` property.

	Documentation [PerformanceTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming>
**/
@:deprecated("PerformanceTiming is deprecated, use the PerformanceNavigationTiming interface instead")
@:native("PerformanceTiming")
extern class PerformanceTiming {
	
	/**
		When the prompt for unload terminates on the previous document in the same browsing context. If there is no previous document, this value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var navigationStart(default,null) : Int;
	
	/**
		When the `unload` event has been thrown, indicating the time at which the previous document in the window began to unload. If there is no previous document, or if the previous document or one of the needed redirects is not of the same origin, the value returned is `0`.
	**/
	var unloadEventStart(default,null) : Int;
	
	/**
		When the `unload` event handler finishes. If there is no previous document, or if the previous document, or one of the needed redirects, is not of the same origin, the value returned is `0`.
	**/
	var unloadEventEnd(default,null) : Int;
	
	/**
		When the first HTTP redirect starts. If there is no redirect, or if one of the redirects is not of the same origin, the value returned is `0`.
	**/
	var redirectStart(default,null) : Int;
	
	/**
		When the last HTTP redirect is completed, that is when the last byte of the HTTP response has been received. If there is no redirect, or if one of the redirects is not of the same origin, the value returned is `0`.
	**/
	var redirectEnd(default,null) : Int;
	
	/**
		When the browser is ready to fetch the document using an HTTP request. This moment is before the check to any application cache.
	**/
	var fetchStart(default,null) : Int;
	
	/**
		When the domain lookup starts. If a persistent connection is used, or the information is stored in a cache or a local resource, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var domainLookupStart(default,null) : Int;
	
	/**
		When the domain lookup is finished. If a persistent connection is used, or the information is stored in a cache or a local resource, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var domainLookupEnd(default,null) : Int;
	
	/**
		When the request to open a connection is sent to the network. If the transport layer reports an error and the connection establishment is started again, the last connection establishment start time is given. If a persistent connection is used, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var connectStart(default,null) : Int;
	
	/**
		When the connection is opened network. If the transport layer reports an error and the connection establishment is started again, the last connection establishment end time is given. If a persistent connection is used, the value will be the same as `PerformanceTiming.fetchStart`. A connection is considered as opened when all secure connection handshake, or SOCKS authentication, is terminated.
	**/
	var connectEnd(default,null) : Int;
	
	/**
		When the secure connection handshake starts. If no such connection is requested, it returns `0`.
	**/
	var secureConnectionStart(default,null) : Int;
	
	/**
		When the browser sent the request to obtain the actual document, from the server or from a cache. If the transport layer fails after the start of the request and the connection is reopened, this property will be set to the time corresponding to the new request.
	**/
	var requestStart(default,null) : Int;
	
	/**
		When the browser received the first byte of the response, from the server from a cache, or from a local resource.
	**/
	var responseStart(default,null) : Int;
	
	/**
		When the browser received the last byte of the response, or when the connection is closed if this happened first, from the server, the cache, or from a local resource.
	**/
	var responseEnd(default,null) : Int;
	
	/**
		When the parser started its work, that is when its `Document.readyState` changes to `'loading'` and the corresponding `readystatechange` event is thrown.
	**/
	var domLoading(default,null) : Int;
	
	/**
		When the parser finished its work on the main document, that is when its `Document.readyState` changes to `'interactive'` and the corresponding `readystatechange` event is thrown.
	**/
	var domInteractive(default,null) : Int;
	
	/**
		Right before the parser sent the `DOMContentLoaded` event, that is right after all the scripts that need to be executed right after parsing have been executed.
	**/
	var domContentLoadedEventStart(default,null) : Int;
	
	/**
		Right after all the scripts that need to be executed as soon as possible, in order or not, have been executed.
	**/
	var domContentLoadedEventEnd(default,null) : Int;
	
	/**
		When the parser finished its work on the main document, that is when its `Document.readyState` changes to `'complete'` and the corresponding `readystatechange` event is thrown.
	**/
	var domComplete(default,null) : Int;
	
	/**
		When the `load` event was sent for the current document. If this event has not yet been sent, it returns `0.`
	**/
	var loadEventStart(default,null) : Int;
	
	/**
		When the `load` event handler terminated, that is when the load event is completed. If this event has not yet been sent, or is not yet completed, it returns `0.`
	**/
	var loadEventEnd(default,null) : Int;
	
	
	/**
		Returns a JSON object representing this `PerformanceTiming` object.
	**/
	function toJSON() : Dynamic;
}