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

// This file is generated from mozilla\Location.webidl. Do not edit!

package js.html;

/**
	The `Location` interface represents the location (URL) of the object it is linked to. Changes done on it are reflected on the object it relates to. Both the `Document` and `Window` interface have such a linked `Location`, accessible via `Document.location` and `Window.location` respectively.

	Documentation [Location](https://developer.mozilla.org/en-US/docs/Web/API/Location) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Location$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Location>
**/
@:native("Location")
extern class Location
{
	
	/**
		Is a `DOMString` containing the entire URL. If changed, the associated document navigates to the new page. It can be set from a different origin than the associated document.
	**/
	var href : String;
	
	/**
		Returns a `DOMString` containing the canonical form of the origin of the specific location.
	**/
	var origin(default,null) : String;
	
	/**
		Is a `DOMString` containing the protocol scheme of the URL, including the final `':'`.
	**/
	var protocol : String;
	
	/**
		Is a `DOMString` containing the host, that is the hostname, a `':'`, and the port of the URL.
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
		Is a `DOMString` containing a `'?'` followed by the parameters of the URL. Also known as "querystring".
	**/
	var search : String;
	
	/**
		Is a `DOMString` containing a `'#'` followed by the fragment identifier of the URL.
	**/
	var hash : String;
	
	/** @throws DOMError */
	
	/**
		Loads the resource at the URL provided in parameter.
	**/
	function assign( url : String ) : Void;
	/** @throws DOMError */
	
	/**
		Replaces the current resource with the one at the provided URL. The difference from the `assign()` method is that after using `replace()` the current page will not be saved in session `History`, meaning the user won't be able to use the back button to navigate to it.
	**/
	function replace( url : String ) : Void;
	/** @throws DOMError */
	
	/**
		Reloads the resource from the current URL. Its optional unique parameter is a `Boolean`, which, when it is `true`, causes the page to always be reloaded from the server. If it is `false` or not specified, the browser may reload the page from its cache.
	**/
	function reload( ?forceget : Bool = false ) : Void;
}