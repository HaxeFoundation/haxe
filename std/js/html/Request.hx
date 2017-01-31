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

// This file is generated from mozilla\Request.webidl. Do not edit!

package js.html;

/**
	The `Request` interface of the Fetch API represents a resource request.

	Documentation [Request](https://developer.mozilla.org/en-US/docs/Web/API/Request) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Request$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Request>
**/
@:native("Request")
extern class Request
{
	
	/**
		Contains the request's method (`GET`, `POST`, etc.)
	**/
	var method(default,null) : String;
	
	/**
		Contains the URL of the request.
	**/
	var url(default,null) : String;
	
	/**
		Contains the associated `Headers` object of the request.
	**/
	var headers(default,null) : Headers;
	
	/**
		Contains the referrer of the request (e.g., `client`).
	**/
	var referrer(default,null) : String;
	
	/**
		Contains the referrer policy of the request (e.g., `no-referrer`).
	**/
	var referrerPolicy(default,null) : ReferrerPolicy;
	
	/**
		Contains the mode of the request (e.g., `cors`, `no-cors`, `same-origin`, `navigate`.)
	**/
	var mode(default,null) : RequestMode;
	
	/**
		Contains the credentials of the request (e.g., `omit`, `same-origin`).
	**/
	var credentials(default,null) : RequestCredentials;
	
	/**
		Contains the cache mode of the request (e.g., `default`, `reload`, `no-cache`).
	**/
	var cache(default,null) : RequestCache;
	
	/**
		Contains the mode for how redirects are handled. It may be one of `follow`, `error`, or `manual`.
	**/
	var redirect(default,null) : RequestRedirect;
	var bodyUsed(default,null) : Bool;
	
	/** @throws DOMError */
	function new( input : haxe.extern.EitherType<Request,String>, ?init : RequestInit ) : Void;
	/** @throws DOMError */
	
	/**
		Creates a copy of the current `Request` object.
	**/
	function clone() : Request;
	/** @throws DOMError */
	function arrayBuffer() : Promise<ArrayBuffer>;
	/** @throws DOMError */
	function blob() : Promise<Blob>;
	/** @throws DOMError */
	function formData() : Promise<FormData>;
	/** @throws DOMError */
	function json() : Promise<Dynamic>;
	/** @throws DOMError */
	function text() : Promise<String>;
}