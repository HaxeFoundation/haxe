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

// This file is generated from mozilla\History.webidl. Do not edit!

package js.html;

/**
	The `History` interface allows to manipulate the browser session history, that is the pages visited in the tab or frame that the current page is loaded in.

	Documentation [History](https://developer.mozilla.org/en-US/docs/Web/API/History) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/History$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/History>
**/
@:native("History")
extern class History
{
	
	/**
		Returns an `Integer` representing the number of elements in the session history, including the currently loaded page. For example, for a page loaded in a new tab this property returns `1`.
	**/
	var length(default,null) : Int;
	
	/**
		Allows web applications to explicitly set default scroll restoration behavior on history navigation. This property can be either `auto` or `manual`.
	**/
	var scrollRestoration : ScrollRestoration;
	
	/**
		Returns an `any` value representing the state at the top of the history stack. This is a way to look at the state without having to wait for a `popstate` event.
	**/
	var state(default,null) : Dynamic;
	
	/** @throws DOMError */
	
	/**
		Loads a page from the session history, identified by its relative location to the current page, for example -1 for the previous page or 1  for the next page. If you specify an out-of-bounds value (for instance, specifying -1 when there are no previously-visited pages in the session history), this method silently has no effect. Calling `go()` without parameters or a value of 0 reloads the current page.
		 Internet Explorer lets you specify a string to go to a specific page in the history list; you should avoid this feature because it is non-standard.
		 
	**/
	function go( ?delta : Int = 0 ) : Void;
	/** @throws DOMError */
	
	/**
		Goes to the previous page in session history, the same action as when the user clicks the browser's Back button. Equivalent to `history.go(-1)`.
		 Calling this method to go back beyond the first page in the session history has no effect and doesn't raise an exception.
		 
	**/
	function back() : Void;
	/** @throws DOMError */
	
	/**
		Goes to the next page in session history, the same action as when the user clicks the browser's Forward button; this is equivalent to `history.go(1)`.
		 Calling this method to go forward beyond the most recent page in the session history has no effect and doesn't raise an exception.
		 
	**/
	function forward() : Void;
	/** @throws DOMError */
	
	/**
		Pushes the given data onto the session history stack with the specified title and, if provided, URL. The data is treated as opaque by the DOM; you may specify any JavaScript object that can be serialized.  Note that Firefox currently ignores the title parameter; for more information, see manipulating the browser history.
	**/
	function pushState( data : Dynamic, title : String, ?url : String ) : Void;
	/** @throws DOMError */
	
	/**
		Updates the most recent entry on the history stack to have the specified data, title, and, if provided, URL. The data is treated as opaque by the DOM; you may specify any JavaScript object that can be serialized.  Note that Firefox currently ignores the title parameter; for more information, see manipulating the browser history.
	**/
	function replaceState( data : Dynamic, title : String, ?url : String ) : Void;
}