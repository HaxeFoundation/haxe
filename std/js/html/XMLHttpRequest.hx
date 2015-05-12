/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/XMLHttpRequest.webidl line 58:0. Do not edit!

package js.html;

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
	var response(default,null) : Dynamic;
	var responseText(default,null) : String;
	var responseXML(default,null) : HTMLDocument;
	
	/** @throws DOMError */
	@:overload( function( ?params : Dynamic/*MISSING MozXMLHttpRequestParameters*/ ) : Void {} )
	function new( ignored : String ) : Void;
	/** @throws DOMError */
	@:overload( function( method : String, url : String ) : Void {} )
	function open( method : String, url : String, async : Bool, ?user : String, ?password : String ) : Void;
	/** @throws DOMError */
	function setRequestHeader( header : String, value : String ) : Void;
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( data : ArrayBuffer ) : Void {} )
	@:overload( function( data : ArrayBufferView ) : Void {} )
	@:overload( function( data : Blob ) : Void {} )
	@:overload( function( data : HTMLDocument ) : Void {} )
	@:overload( function( data : String ) : Void {} )
	@:overload( function( data : FormData ) : Void {} )
	function send( data : Dynamic/*MISSING InputStream*/ ) : Void;
	/** @throws DOMError */
	function abort() : Void;
	/** @throws DOMError */
	function getResponseHeader( header : String ) : String;
	/** @throws DOMError */
	function getAllResponseHeaders() : String;
	/** @throws DOMError */
	function overrideMimeType( mime : String ) : Void;
	/** @throws DOMError */
	function sendAsBinary( body : String ) : Void;
}