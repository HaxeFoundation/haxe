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

// This file is generated from mozilla\ErrorEvent.webidl. Do not edit!

package js.html;

/**
	The `ErrorEvent` interface represents events providing information related to errors in scripts or in files.

	Documentation [ErrorEvent](https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent>
**/
@:native("ErrorEvent")
extern class ErrorEvent extends Event
{
	
	/**
		Is a `DOMString` containing a human-readable error message describing the problem.
	**/
	var message(default,null) : String;
	
	/**
		Is a `DOMString` containing the name of the script file in which the error occurred.
	**/
	var filename(default,null) : String;
	
	/**
		Is an `integer` containing the line number of the script file on which the error occurred.
	**/
	var lineno(default,null) : Int;
	
	/**
		Is an `integer` containing the column number of the script file on which the error occurred.
	**/
	var colno(default,null) : Int;
	
	/**
		Is a JavaScript `Object` that is concerned by the event.
	**/
	var error(default,null) : Dynamic;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : ErrorEventInit ) : Void;
}