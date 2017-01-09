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

// This file is generated from mozilla\XMLHttpRequest.webidl. Do not edit!

package js.html;

/**
	`XMLHttpRequest` is an API that provides client functionality for transferring data between a client and a server. It provides an easy way to retrieve data from a URL without having to do a full page refresh. This enables a Web page to update just a part of the page without disrupting what the user is doing.

	Documentation [XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest>
**/
@:native("XMLHttpRequest")
extern class XMLHttpRequest extends XMLHttpRequestEventTarget
{
	static inline var UNSENT : Int = 0;
	static inline var OPENED : Int = 1;
	static inline var HEADERS_RECEIVED : Int = 2;
	static inline var LOADING : Int = 3;
	static inline var DONE : Int = 4;
	
	var onreadystatechange : haxe.Constraints.Function;
	var readyState(default,null) : Int;
	var timeout : Int;
	var withCredentials : Bool;
	var upload(default,null) : XMLHttpRequestUpload;
	var responseURL(default,null) : String;
	var status(default,null) : Int;
	var statusText(default,null) : String;
	var responseType : XMLHttpRequestResponseType;
	
	/**
		Returns an `ArrayBuffer`, `Blob`, `Document`, JavaScript object, or a `DOMString`, depending on the value of `XMLHttpRequest.responseType`. that contains the response entity body.
	**/
	var response(default,null) : Dynamic;
	var responseText(default,null) : String;
	var responseXML(default,null) : HTMLDocument;
	
	/** @throws DOMError */
	@:overload( function( ?params : Dynamic/*MISSING MozXMLHttpRequestParameters*/ ) : Void {} )
	function new( ignored : String ) : Void;
	/** @throws DOMError */
	@:overload( function( method : String, url : String ) : Void {} )
	
	/**
		Initializes a request. This method is to be used from JavaScript code; to initialize a request from native code, use `openRequest()` instead.
	**/
	function open( method : String, url : String, async : Bool, ?user : String, ?password : String ) : Void;
	/** @throws DOMError */
	
	/**
		Sets the value of an HTTP request header. You must call `setRequestHeader()`after `open()`, but before `send()`.
	**/
	function setRequestHeader( header : String, value : String ) : Void;
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( data : ArrayBuffer ) : Void {} )
	@:overload( function( data : ArrayBufferView ) : Void {} )
	@:overload( function( data : Blob ) : Void {} )
	@:overload( function( data : HTMLDocument ) : Void {} )
	@:overload( function( data : String ) : Void {} )
	@:overload( function( data : FormData ) : Void {} )
	
	/**
		Sends the request. If the request is asynchronous (which is the default), this method returns as soon as the request is sent.
	**/
	function send( data : Dynamic/*MISSING InputStream*/ ) : Void;
	/** @throws DOMError */
	
	/**
		Aborts the request if it has already been sent.
	**/
	function abort() : Void;
	/** @throws DOMError */
	
	/**
		Returns the string containing the text of the specified header, or `null` if either the response has not yet been received or the header doesn't exist in the response.
	**/
	function getResponseHeader( header : String ) : String;
	/** @throws DOMError */
	
	/**
		Returns all the response headers, separated by CRLF, as a string, or `null` if no response has been received. 
	**/
	function getAllResponseHeaders() : String;
	/** @throws DOMError */
	
	/**
		Overrides the MIME type returned by the server.
	**/
	function overrideMimeType( mime : String ) : Void;
}