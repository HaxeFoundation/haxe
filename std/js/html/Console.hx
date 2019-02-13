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

// This file is generated from mozilla\Console.webidl. Do not edit!

package js.html;

/**
	The `Console` object provides access to the browser's debugging console (e.g. the Web Console in Firefox). The specifics of how it works varies from browser to browser, but there is a de facto set of features that are typically provided.

	Documentation [console](https://developer.mozilla.org/en-US/docs/Web/API/console) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/console$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/console>
**/
@:native("console")
extern class Console {
	
	/**
		Log a message and stack trace to console if the first argument is `false`.
	**/
	static function assert( condition : Bool = false, data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Clear the console.
	**/
	static function clear() : Void;
	
	/**
		Log the number of times this line has been called with the given label.
	**/
	static function count( label : String = "default" ) : Void;
	
	/**
		Resets the value of the counter with the given label.
	**/
	static function countReset( label : String = "default" ) : Void;
	
	/**
		Outputs a message to the console with the log level `"debug"`.
		 Note: Starting with Chromium 58 this method only appears in Chromium browser consoles when level "Verbose" is selected.
		 
	**/
	static function debug( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs an error message. You may use string substitution and additional arguments with this method.
	**/
	static function error( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Informative logging of information. You may use string substitution and additional arguments with this method.
	**/
	static function info( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		For general output of logging information. You may use string substitution and additional arguments with this method.
	**/
	static function log( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Displays tabular data as a table.
	**/
	static function table( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs a stack trace.
	**/
	static function trace( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Outputs a warning message. You may use string substitution and additional arguments with this method.
	**/
	static function warn( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Displays an interactive listing of the properties of a specified JavaScript object. This listing lets you use disclosure triangles to examine the contents of child objects.
	**/
	static function dir( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		
		 Displays an XML/HTML Element representation of the specified object if possible or the JavaScript Object view if it is not possible.
		 
	**/
	static function dirxml( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Creates a new inline group, indenting all following output by another level. To move back out a level, call `groupEnd()`.
	**/
	static function group( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Creates a new inline group, indenting all following output by another level. However, unlike `group()` this starts with the inline group collapsed requiring the use of a disclosure button to expand it. To move back out a level, call `groupEnd()`.
	**/
	static function groupCollapsed( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Exits the current inline group.
	**/
	static function groupEnd() : Void;
	
	/**
		Starts a timer with a name specified as an input parameter. Up to 10,000 simultaneous timers can run on a given page.
	**/
	static function time( label : String = "default" ) : Void;
	
	/**
		Logs the value of the specified timer to the console.
	**/
	static function timeLog( label : String = "default", data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Stops the specified timer and logs the elapsed time in seconds since it started.
	**/
	static function timeEnd( label : String = "default" ) : Void;
	
	/**
		An alias for `error()`.
	**/
	static function exception( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Adds a marker to the browser's Timeline or Waterfall tool.
	**/
	static function timeStamp( ?data : Dynamic ) : Void;
	
	/**
		Starts the browser's built-in profiler (for example, the Firefox performance tool). You can specify an optional name for the profile.
	**/
	static function profile( data : haxe.extern.Rest<Dynamic> ) : Void;
	
	/**
		Stops the profiler. You can see the resulting profile in the browser's performance tool (for example, the Firefox performance tool).
	**/
	static function profileEnd( data : haxe.extern.Rest<Dynamic> ) : Void;
}