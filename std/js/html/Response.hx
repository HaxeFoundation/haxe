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

// This file is generated from mozilla\Response.webidl. Do not edit!

package js.html;

@:native("Response")
extern class Response
{
	static function error() : Response;
	/** @throws DOMError */
	static function redirect( url : String, ?status : Int = 302 ) : Response;
	var type(default,null) : ResponseType;
	var url(default,null) : String;
	var status(default,null) : Int;
	var ok(default,null) : Bool;
	var statusText(default,null) : String;
	var headers(default,null) : Headers;
	var bodyUsed(default,null) : Bool;
	
	/** @throws DOMError */
	function new( ?body : haxe.extern.EitherType<ArrayBuffer,haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,haxe.extern.EitherType<FormData,haxe.extern.EitherType<String,URLSearchParams>>>>>, ?init : ResponseInit ) : Void;
	/** @throws DOMError */
	function clone() : Response;
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