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

// This file is generated from mozilla/WorkerGlobalScope.webidl line 16:0. Do not edit!

package js.html;

@:native("WorkerGlobalScope")
extern class WorkerGlobalScope extends EventTarget
{
	var self(default,null) : WorkerGlobalScope;
	var console(default,null) : Console;
	var location(default,null) : WorkerLocation;
	var onerror : haxe.extern.EitherType<Event,String> -> String -> Int -> Int -> Dynamic -> Bool;
	var onoffline : haxe.Constraints.Function;
	var ononline : haxe.Constraints.Function;
	var navigator(default,null) : WorkerNavigator;
	var onclose : haxe.Constraints.Function;
	var performance(default,null) : Performance;
	
	function close() : Void;
	/** @throws DOMError */
	function importScripts( urls : haxe.extern.Rest<String> ) : Void;
	function dump( ?str : String ) : Void;
	/** @throws DOMError */
	function btoa( btoa : String ) : String;
	/** @throws DOMError */
	function atob( atob : String ) : String;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setTimeout( handler : String, ?timeout : Int = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	/** @throws DOMError */
	function clearTimeout( ?handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setInterval( handler : String, ?timeout : Int, unused : haxe.extern.Rest<Dynamic> ) : Int;
	/** @throws DOMError */
	function clearInterval( ?handle : Int = 0 ) : Void;
}