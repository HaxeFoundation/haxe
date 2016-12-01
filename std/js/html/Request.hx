/*
 * Copyright (C)2005-2016 Haxe Foundation
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

@:native("Request")
extern class Request
{
	var method(default,null) : String;
	var url(default,null) : String;
	var headers(default,null) : Headers;
	var referrer(default,null) : String;
	var referrerPolicy(default,null) : ReferrerPolicy;
	var mode(default,null) : RequestMode;
	var credentials(default,null) : RequestCredentials;
	var cache(default,null) : RequestCache;
	var redirect(default,null) : RequestRedirect;
	var bodyUsed(default,null) : Bool;
	
	/** @throws DOMError */
	function new( input : haxe.extern.EitherType<Request,String>, ?init : RequestInit ) : Void;
	/** @throws DOMError */
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