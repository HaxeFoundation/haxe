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

// This file is generated from mozilla\Console.webidl. Do not edit!

package js.html;

/**
	The `Console` object provides access to the browser's debugging console (e.g., the Web Console in Firefox). The specifics of how it works vary from browser to browser, but there is a de facto set of features that are typically provided.

	Documentation [Console](https://developer.mozilla.org/en-US/docs/Web/API/Console) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Console$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Console>
**/
@:native("Console")
extern class Console
{
	
	/**
		For general output of logging information. You may use string substitution and additional arguments with this method.
	**/
	function log( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Informative logging information. You may use string substitution and additional arguments with this method.
	**/
	function info( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs a warning message. You may use string substitution and additional arguments with this method.
	**/
	function warn( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs an error message. You may use string substitution and additional arguments with this method.
	**/
	function error( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		An alias for `error()`
	**/
	function exception( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		An alias for `log()`
	**/
	function debug( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Displays tabular data as a table.
	**/
	function table( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs a stack trace.
	**/
	function trace() : Void;
	
	/**
		Displays an interactive listing of the properties of a specified JavaScript object. This listing lets you use disclosure triangles to examine the contents of child objects.
	**/
	function dir( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		
		 Displays an XML/HTML Element representation of the specified object if possible or the JavaScript Object view if it is not.
		 
	**/
	function dirxml( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Creates a new inline group, indenting all following output by another level. To move back out a level, call `groupEnd()`.
	**/
	function group( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Creates a new inline group, indenting all following output by another level; unlike `group()`, this starts with the inline group collapsed, requiring the use of a disclosure button to expand it. To move back out a level, call `groupEnd()`.
	**/
	function groupCollapsed( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Exits the current inline group.
	**/
	function groupEnd( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Starts a timer with a name specified as an input parameter. Up to 10,000 simultaneous timers can run on a given page.
	**/
	function time( ?time : Dynamic ) : Void;
	
	/**
		Stops the specified timer and logs the elapsed time in seconds since its start.
	**/
	function timeEnd( ?time : Dynamic ) : Void;
	
	/**
		Adds a marker to the browser's Timeline or Waterfall tool.
	**/
	function timeStamp( ?data : Dynamic ) : Void;
	
	/**
		Starts the browser's build-in profiler (for example, the Firefox performance tool). You can specify an optional name for the profile.
	**/
	function profile( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Stops the profiler. You can see the resulting profile in the browser's performance tool (for example, the Firefox performance tool).
	**/
	function profileEnd( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Log a message and stack trace to console if first argument is `false`.
	**/
	function assert( condition : Bool, data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Log the number of times this line has been called with the given label.
	**/
	function count( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Clear the console.
	**/
	function clear() : Void;
	function markTimeline() : Void;
	function timeline() : Void;
	function timelineEnd() : Void;
}