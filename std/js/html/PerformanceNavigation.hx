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

// This file is generated from mozilla\PerformanceNavigation.webidl. Do not edit!

package js.html;

/**
	The legacy `PerformanceNavigation` interface represents information about how the navigation to the current document was done.

	Documentation [PerformanceNavigation](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation>
**/
@:native("PerformanceNavigation")
extern class PerformanceNavigation {
	static inline 
	/**
		
	**/
	var TYPE_NAVIGATE : Int = 0;
	static inline 
	/**
		
	**/
	var TYPE_RELOAD : Int = 1;
	static inline 
	/**
		
	**/
	var TYPE_BACK_FORWARD : Int = 2;
	static inline 
	/**
		An `unsigned short` which indicates how the navigation to this page was done. Possible values are:
		 
		  `TYPE_NAVIGATE` (0)
		  The page was accessed by following a link, a bookmark, a form submission, or a script, or by typing the URL in the address bar.
		  `TYPE_RELOAD` (1)
		  The page was accessed by clicking the Reload button or via the `Location.reload()` method.
		  `TYPE_BACK_FORWARD` (2)
		  The page was accessed by navigating into the history.
		  `TYPE_RESERVED` (255)
		  Any other way.
		 
		 
	**/
	var TYPE_RESERVED : Int = 255;
	
	
	/**
		
	**/
	var type(default,null) : Int;
	
	/**
		An `unsigned short` representing the number of REDIRECTs done before reaching the page.
	**/
	var redirectCount(default,null) : Int;
	
	
	/**
		Is a jsonizer returning a json object representing the `PerformanceNavigation` object.
	**/
	function toJSON() : Dynamic;
}