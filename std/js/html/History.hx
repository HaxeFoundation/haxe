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

// This file is generated from mozilla\History.webidl. Do not edit!

package js.html;

@:native("History")
extern class History
{
	var length(default,null) : Int;
	var scrollRestoration : ScrollRestoration;
	var state(default,null) : Dynamic;
	
	/** @throws DOMError */
	function go( ?delta : Int = 0 ) : Void;
	/** @throws DOMError */
	function back() : Void;
	/** @throws DOMError */
	function forward() : Void;
	/** @throws DOMError */
	function pushState( data : Dynamic, title : String, ?url : String ) : Void;
	/** @throws DOMError */
	function replaceState( data : Dynamic, title : String, ?url : String ) : Void;
}