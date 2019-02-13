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

// This file is generated from mozilla\PerformanceResourceTiming.webidl. Do not edit!

package js.html;

/**
	The `PerformanceResourceTiming` interface enables retrieval and analysis of detailed network timing data regarding the loading of an application's resources. An application can use the timing metrics to determine, for example, the length of time it takes to fetch a specific resource, such as an `XMLHttpRequest`, `SVG`, image, or script.

	Documentation [PerformanceResourceTiming](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceResourceTiming) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceResourceTiming$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceResourceTiming>
**/
@:native("PerformanceResourceTiming")
extern class PerformanceResourceTiming extends PerformanceEntry {
	
	/**
		A `DOMString` representing the type of resource that initiated the performance entry, as specified in `PerformanceResourceTiming.initiatorType`.
	**/
	var initiatorType(default,null) : String;
	
	/**
		A `DOMString` representing the network protocol used to fetch the resource, as identified by the ALPN Protocol ID (RFC7301).
	**/
	var nextHopProtocol(default,null) : String;
	
	/**
		Returns a `DOMHighResTimeStamp` immediately before dispatching the `FetchEvent` if a Service Worker thread is already running, or immediately before starting the Service Worker thread if it is not already running. If the resource is not intercepted by a Service Worker the property will always return 0.
	**/
	var workerStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` that represents the start time of the fetch which initiates the redirect.
	**/
	var redirectStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately after receiving the last byte of the response of the last redirect.
	**/
	var redirectEnd(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately before the browser starts to fetch the resource.
	**/
	var fetchStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately before the browser starts the domain name lookup for the resource.
	**/
	var domainLookupStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` representing the time immediately after the browser finishes the domain name lookup for the resource.
	**/
	var domainLookupEnd(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately before the browser starts to establish the connection to the server to retrieve the resource.
	**/
	var connectStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately after the browser finishes establishing the connection to the server to retrieve the resource.
	**/
	var connectEnd(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately before the browser starts the handshake process to secure the current connection.
	**/
	var secureConnectionStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately before the browser starts requesting the resource from the server.
	**/
	var requestStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately after the browser receives the first byte of the response from the server.
	**/
	var responseStart(default,null) : Float;
	
	/**
		A `DOMHighResTimeStamp` immediately after the browser receives the last byte of the resource or immediately before the transport connection is closed, whichever comes first.
	**/
	var responseEnd(default,null) : Float;
	
	/**
		A `number` representing the size (in octets) of the fetched resource. The size includes the response header fields plus the response payload body.
	**/
	var transferSize(default,null) : Int;
	
	/**
		A `number` representing the size (in octets) received from the fetch (HTTP or cache), of the payload body, before removing any applied content-codings.
	**/
	var encodedBodySize(default,null) : Int;
	
	/**
		A `number` that is the size (in octets) received from the fetch (HTTP or cache) of the message body, after removing any applied content-codings.
	**/
	var decodedBodySize(default,null) : Int;
	
	/**
		An array of `PerformanceServerTiming` entries containing server timing metrics.
	**/
	var serverTiming(default,null) : Array<PerformanceServerTiming>;
	
	
	/**
		Returns a `DOMString` that is the JSON representation of the `PerformanceResourceTiming` object.
	**/
	function toJSON() : Dynamic;
}