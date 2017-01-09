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

// This file is generated from mozilla\PerformanceTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceTiming` interface represents timing-related performance information for the given page.

	Documentation [PerformanceTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming>
**/
@:native("PerformanceTiming")
extern class PerformanceTiming
{
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, right after the prompt for unload terminates on the previous document in the same browsing context. If there is no previous document, this value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var navigationStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, the `unload` event has been thrown. If there is no previous document, or if the previous document, or one of the needed redirects, is not of the same origin, the value returned is `0`.
	**/
	var unloadEventStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, the `unload` event handler finishes. If there is no previous document, or if the previous document, or one of the needed redirects, is not of the same origin, the value returned is `0`.
	**/
	var unloadEventEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, the first HTTP redirect starts. If there is no redirect, or if one of the redirects is not of the same origin, the value returned is `0`.
	**/
	var redirectStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, the last HTTP redirect is completed, that is when the last byte of the HTTP response has been received. If there is no redirect, or if one of the redirect is not of the same origin, the value returned is `0`.
	**/
	var redirectEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, the browser is ready to fetch the document using an HTTP request. This moment is before the check to any application cache.
	**/
	var fetchStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, where the domain lookup starts. If a persistent connection is used, or the information is stored in a cache or a local resource, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var domainLookupStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, where the domain lookup is finished. If a persistent connection is used, or the information is stored in a cache or a local resource, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var domainLookupEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, where the request to open a connection is sent to the network. If the transport layer reports an error and the connection establishment is started again, the last connection establisment start time is given. If a persistent connection is used, the value will be the same as `PerformanceTiming.fetchStart`.
	**/
	var connectStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, where the connection is opened network. If the transport layer reports an error and the connection establishment is started again, the last connection establisment end time is given. If a persistent connection is used, the value will be the same as `PerformanceTiming.fetchStart`. A connection is considered as opened when all secure connection handshake, or SOCKS authentication, is terminated.
	**/
	var connectEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the browser sent the request to obtain the actual document, from the server or from a cache. If the transport layer fails after the start of the request and the connection is reopened, this property will be set to the time corresponding to the new request.
	**/
	var requestStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the browser received the first byte of the response, from the server from a cache, of from a local resource.
	**/
	var responseStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the browser received the last byte of the response, or when the connection is closed if this happened first, from the server from a cache, of from a local resource.
	**/
	var responseEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the parser started its work, that is when its `Document.readyState` changes to `'loading'` and the corresponding `readystatechange` event is thrown.
	**/
	var domLoading(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the parser finished its work on the main document, that is when its `Document.readyState` changes to `'interactive'` and the corresponding `readystatechange` event is thrown.
	**/
	var domInteractive(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, right before the parser sent the `DOMContentLoaded` event, that is right after all the scripts that need to be executed right after parsing has been executed.
	**/
	var domContentLoadedEventStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, right after all the scripts that need to be executed as soon as possible, in order or not, has been executed.
	**/
	var domContentLoadedEventEnd(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the parser finished its work on the main document, that is when its `Document.readyState` changes to `'complete'` and the corresponding `readystatechange` event is thrown.
	**/
	var domComplete(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the `load` event was sent for the current document. If this event has not yet been sent, it returns `0.`
	**/
	var loadEventStart(default,null) : Int;
	
	/**
		Is an `unsigned long long` representing the moment, in miliseconds since the UNIX epoch, when the `load` event handler terminated, that is when the load event is completed. If this event has not yet been sent, or is not yet completed, it returns `0.`
	**/
	var loadEventEnd(default,null) : Int;
	
}