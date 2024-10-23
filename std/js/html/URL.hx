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

// This file is generated from mozilla\URL.webidl. Do not edit!

package js.html;

import haxe.extern.EitherType;

/**
	The URL interface represents an object providing static methods used for creating object URLs.

	Documentation [URL](https://developer.mozilla.org/en-US/docs/Web/API/URL) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/URL$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/URL>
**/
@:native("URL")
extern class URL {
	/** @throws DOMError */
	@:overload( function( blob : Blob ) : String {} )
	static function createObjectURL( source : MediaSource ) : String;

	/** @throws DOMError */
	static function revokeObjectURL( url : String ) : Void;

	/**
		Returns a boolean indicating whether or not an absolute URL, or a relative URL combined with a base URL, are parsable and valid.
	**/
	static function canParse( url : String, ?base : EitherType<String, URL> ) : Bool;

	/**
		Returns a newly created `URL` object representing the URL defined by the parameters.
	**/
	static function parse( url : String, ?base : EitherType<String, URL> ): Null<URL>;

	/**
		Is a `DOMString` containing the whole URL.
	**/
	var href : String;
	
	/**
		Returns a `DOMString` containing the origin of the URL, that is its scheme, its domain and its port.
	**/
	var origin(default,null) : String;
	
	/**
		Is a `DOMString` containing the protocol scheme of the URL, including the final `':'`.
	**/
	var protocol : String;
	
	/**
		Is a `DOMString` containing the username specified before the domain name.
	**/
	var username : String;
	
	/**
		Is a `DOMString` containing the password specified before the domain name.
	**/
	var password : String;
	
	/**
		Is a `DOMString` containing the domain (that is the hostname) followed by (if a port was specified) a `':'` and the port of the URL.
	**/
	var host : String;
	
	/**
		Is a `DOMString` containing the domain of the URL.
	**/
	var hostname : String;
	
	/**
		Is a `DOMString` containing the port number of the URL.
	**/
	var port : String;
	
	/**
		Is a `DOMString` containing an initial `'/'` followed by the path of the URL.
	**/
	var pathname : String;
	
	/**
		Is a `DOMString` containing a `'?'` followed by the parameters of the URL.
	**/
	var search : String;
	
	/**
		Returns a `URLSearchParams` object allowing to access the GET query arguments contained in the URL.
	**/
	var searchParams(default,null) : URLSearchParams;
	
	/**
		Is a `DOMString` containing a `'#'` followed by the fragment identifier of the URL.
	**/
	var hash : String;
	
	/** @throws DOMError */
	function new( url : String, ?base : EitherType<String, URL> );

	/**
		Returns a JSON representation of this URL.
	**/
	function toJSON() : String;

	/**
		Returns a string representation of this URL.
	**/
	function toString() : String;
}
