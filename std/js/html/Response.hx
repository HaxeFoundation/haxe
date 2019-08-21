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

// This file is generated from mozilla\Response.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `Response` interface of the Fetch API represents the response to a request.

	Documentation [Response](https://developer.mozilla.org/en-US/docs/Web/API/Response) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Response$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Response>
**/
@:native("Response")
extern class Response {

	/**
		Returns a new `Response` object associated with a network error.
	**/
	static function error() : Response;

	/**
		Creates a new response with a different URL.
		@throws DOMError
	**/
	static function redirect( url : String, status : Int = 302 ) : Response;

	/**
		Contains the type of the response (e.g., `basic`, `cors`).
	**/
	var type(default,null) : ResponseType;

	/**
		Contains the URL of the response.
	**/
	var url(default,null) : String;

	/**
		Indicates whether or not the response is the result of a redirect; that is, its URL list has more than one entry.
	**/
	var redirected(default,null) : Bool;

	/**
		Contains the status code of the response (e.g., `200` for a success).
	**/
	var status(default,null) : Int;

	/**
		Contains a boolean stating whether the response was successful (status in the range 200-299) or not.
	**/
	var ok(default,null) : Bool;

	/**
		Contains the status message corresponding to the status code (e.g., `OK` for `200`).
	**/
	var statusText(default,null) : String;

	/**
		Contains the `Headers` object associated with the response.
	**/
	var headers(default,null) : Headers;
	var bodyUsed(default,null) : Bool;

	/** @throws DOMError */
	@:overload( function( ?body : js.lib.ArrayBufferView, ?init : ResponseInit) : Response {} )
	@:overload( function( ?body : js.lib.ArrayBuffer, ?init : ResponseInit) : Response {} )
	@:overload( function( ?body : FormData, ?init : ResponseInit) : Response {} )
	@:overload( function( ?body : URLSearchParams, ?init : ResponseInit) : Response {} )
	@:overload( function( ?body : Dynamic/*MISSING ReadableStream*/, ?init : ResponseInit) : Response {} )
	@:overload( function( ?body : String, ?init : ResponseInit) : Response {} )
	function new( ?body : Blob, ?init : ResponseInit ) : Void;

	/**
		Creates a clone of a `Response` object.
		@throws DOMError
	**/
	function clone() : Response;
	/** @throws DOMError */
	function arrayBuffer() : Promise<js.lib.ArrayBuffer>;
	/** @throws DOMError */
	function blob() : Promise<Blob>;
	/** @throws DOMError */
	function formData() : Promise<FormData>;
	/** @throws DOMError */
	function json() : Promise<Dynamic>;
	/** @throws DOMError */
	function text() : Promise<String>;
}