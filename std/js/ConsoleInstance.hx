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

package js;

/**
	The type of the `console` object, for when it is used as a value (`js.Browser.console`).
	`js.Console` has the same methods as statics.

	@see <https://developer.mozilla.org/docs/Web/API/console>
**/
extern interface ConsoleInstance {
	/**
		The **`console.assert()`** static method writes an error message to the console if the assertion is false. If the assertion is true, nothing happens.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/assert_static>
	**/
	function assert(?condition:Bool = false, data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.clear()`** static method clears the console if possible.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/clear_static>
	**/
	function clear():Void;
	/**
		The **`console.count()`** static method logs the number of times that this particular call to `count()` has been called.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/count_static>
	**/
	function count(?label:String = "default"):Void;
	/**
		The **`console.countReset()`** static method resets counter used with `console.count()`.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/countReset_static>
	**/
	function countReset(?label:String = "default"):Void;
	/**
		The **`console.debug()`** static method outputs a message to the console at the "debug" log level. The message is only displayed to the user if the console is configured to display debug output. In most cases, the log level is configured within the console UI. This log level might correspond to the `Debug` or `Verbose` log level.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/debug_static>
	**/
	function debug(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.dir()`** static method displays a list of the properties of the specified JavaScript object. In browser consoles, the output is presented as a hierarchical listing with disclosure triangles that let you see the contents of child objects.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/dir_static>
	**/
	function dir(?item:Null<Any>, ?options:Null<{ }>):Void;
	/**
		The **`console.dirxml()`** static method displays an interactive tree of the descendant elements of the specified XML/HTML element. If it is not possible to display as an element the JavaScript Object view is shown instead. The output is presented as a hierarchical listing of expandable nodes that let you see the contents of child nodes.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/dirxml_static>
	**/
	function dirxml(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.error()`** static method outputs a message to the console at the "error" log level. The message is only displayed to the user if the console is configured to display error output. In most cases, the log level is configured within the console UI. The message may be formatted as an error, with red colors and call stack information.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/error_static>
	**/
	function error(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.group()`** static method creates a new inline group in the [Web console](https://firefox-source-docs.mozilla.org/devtools-user/web_console/index.html) log, causing any subsequent console messages to be indented by an additional level, until `console.groupEnd()` is called.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/group_static>
	**/
	function group(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.groupCollapsed()`** static method creates a new inline group in the console. Unlike `console.group()`, however, the new group is created collapsed. The user will need to use the disclosure button next to it to expand it, revealing the entries created in the group.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/groupCollapsed_static>
	**/
	function groupCollapsed(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.groupEnd()`** static method exits the current inline group in the console. See [Using groups in the console](https://developer.mozilla.org/en-US/docs/Web/API/console#using_groups_in_the_console) in the `console` documentation for details and examples.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/groupEnd_static>
	**/
	function groupEnd():Void;
	/**
		The **`console.info()`** static method outputs a message to the console at the "info" log level. The message is only displayed to the user if the console is configured to display info output. In most cases, the log level is configured within the console UI. The message may receive special formatting, such as a small "i" icon next to it.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/info_static>
	**/
	function info(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.log()`** static method outputs a message to the console.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/log_static>
	**/
	function log(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.table()`** static method displays tabular data as a table.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/table_static>
	**/
	function table(?tabularData:Null<Any>, ?properties:Array<String>):Void;
	/**
		The **`console.time()`** static method starts a timer you can use to track how long an operation takes. You give each timer a unique name, and may have up to 10,000 timers running on a given page. When you call `console.timeEnd()` with the same name, the browser will output the time, in milliseconds, that elapsed since the timer was started.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/time_static>
	**/
	function time(?label:String = "default"):Void;
	/**
		The **`console.timeEnd()`** static method stops a timer that was previously started by calling `console.time()`.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/timeEnd_static>
	**/
	function timeEnd(?label:String = "default"):Void;
	/**
		The **`console.timeLog()`** static method logs the current value of a timer that was previously started by calling `console.time()`.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/timeLog_static>
	**/
	function timeLog(?label:String = "default", data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.trace()`** static method outputs a stack trace to the console.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/trace_static>
	**/
	function trace(data:haxe.Rest<Null<Any>>):Void;
	/**
		The **`console.warn()`** static method outputs a warning message to the console at the "warning" log level. The message is only displayed to the user if the console is configured to display warning output. In most cases, the log level is configured within the console UI. The message may receive special formatting, such as yellow colors and a warning icon.
		
		@see <https://developer.mozilla.org/docs/Web/API/console/warn_static>
	**/
	function warn(data:haxe.Rest<Null<Any>>):Void;

	/**
		Starts the browser's built-in profiler, or Node.js' inspector profiler. Non-standard, but widely available.
	**/
	function profile(?label:String):Void;
	/**
		Stops the profiler started by `profile()`. Non-standard, but widely available.
	**/
	function profileEnd(?label:String):Void;
	/**
		Adds a marker to the browser's performance tool. Non-standard, but widely available.
	**/
	function timeStamp(?label:String):Void;
	/**
		Non-standard alias of `error()`, only in older Firefox versions.
	**/
	@:deprecated("Use error() instead")
	function exception(data:haxe.Rest<Null<Any>>):Void;
}
