/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.console;

import haxe.extern.EitherType;
import haxe.extern.Rest;
import js.node.stream.Writable;

/**
	The `Console` class can be used to create a simple logger with configurable output streams
	and can be accessed using either `require('console').Console` or `console.Console` (or their destructured counterparts):

	@see https://nodejs.org/api/console.html#console_class_console
**/
@:jsRequire("console", "Console")
extern class Console {
	/**
		Creates a new `Console` with one or two writable stream instances. `stdout` is a writable stream to print log or info output.
		`stderr` is used for warning or error output. If `stderr` is not provided, `stdout` is used for stderr.

		@see https://nodejs.org/api/console.html#console_new_console_stdout_stderr_ignoreerrors
	**/
	@:overload(function(options:ConsoleOptions):Void {})
	function new(stdout:IWritable, ?stderr:IWritable, ?ignoreerrors:Bool):Void;

	/**
		A simple assertion test that verifies whether `value` is truthy. If it is not, `Assertion` failed is logged.
		If provided, the error `message` is formatted using `util.format()` by passing along all message arguments. The output is used as the error message.

		@see https://nodejs.org/api/console.html#console_console_assert_value_message
	**/
	function assert(value:Dynamic, message:Rest<Dynamic>):Void;

	/**
		When `stdout` is a TTY, calling `console.clear()` will attempt to clear the TTY. When `stdout` is not a TTY, this method does nothing.

		@see https://nodejs.org/api/console.html#console_console_clear
	**/
	function clear():Void;

	/**
		Maintains an internal counter specific to `label` and outputs to `stdout` the number of times `console.count()` has been called with the given `label`.

		@see https://nodejs.org/api/console.html#console_console_count_label
	**/
	function count(?label:String):Void;

	/**
		Resets the internal counter specific to `label`.

		@see https://nodejs.org/api/console.html#console_console_countreset_label
	**/
	function countReset(?label:String):Void;

	/**
		The `console.debug()` function is an alias for `console.log()`.

		@see https://nodejs.org/api/console.html#console_console_debug_data_args
	**/
	function debug(data:Dynamic, args:Rest<Dynamic>):Void;

	/**
		Uses util.inspect() on `obj` and prints the resulting string to `stdout`. This function bypasses any custom `inspect()` function defined on `obj`.

		@see https://nodejs.org/api/console.html#console_console_dir_obj_options
	**/
	function dir(obj:Dynamic, ?options:Util.InspectOptionsBase):Void;

	/**
		This method calls `console.log()` passing it the arguments received. This method does not produce any XML formatting.

		@see https://nodejs.org/api/console.html#console_console_dirxml_data
	**/
	function dirxml(data:Rest<Dynamic>):Void;

	/**
		Prints to `stderr` with newline. Multiple arguments can be passed,
		with the first used as the primary message and all additional used as substitution values similar to printf(3)
		(the arguments are all passed to util.format()).

		@see https://nodejs.org/api/console.html#console_console_error_data_args
	**/
	function error(data:Dynamic, args:Rest<Dynamic>):Void;

	/**
		If one or more `label`s are provided, those are printed first without the additional indentation.

		@see https://nodejs.org/api/console.html#console_console_group_label
	**/
	function group(label:Rest<Dynamic>):Void;

	/**
		An alias for console.group().

		@see https://nodejs.org/api/console.html#console_console_groupcollapsed
	**/
	function groupCollapsed():Void;

	/**
		Decreases indentation of subsequent lines by two spaces.

		@see https://nodejs.org/api/console.html#console_console_groupend
	**/
	function groupEnd():Void;

	/**
		The `console.info()` function is an alias for console.log().

		@see https://nodejs.org/api/console.html#console_console_info_data_args
	**/
	function info(data:Dynamic, args:Rest<Dynamic>):Void;

	/**
		Prints to `stdout` with newline. Multiple arguments can be passed,
		with the first used as the primary message and all additional used as substitution values similar to printf(3)
		(the arguments are all passed to util.format()).

		@see https://nodejs.org/api/console.html#console_console_log_data_args
	**/
	function log(data:Dynamic, args:Rest<Dynamic>):Void;

	/**
		Try to construct a table with the columns of the properties of `tabularData` (or use `properties`)
		and rows of `tabularData` and log it. Falls back to just logging the argument if it can’t be parsed as tabular.

		@see https://nodejs.org/api/console.html#console_console_table_tabulardata_properties
	**/
	function table(tabularData:Dynamic, ?properties:Array<String>):Void;

	/**
		Starts a timer that can be used to compute the duration of an operation. Timers are identified by a unique `label`.
		Use the same `label` when calling console.timeEnd() to stop the timer and output the elapsed time in milliseconds to `stdout`.
		Timer durations are accurate to the sub-millisecond.

		@see https://nodejs.org/api/console.html#console_console_time_label
	**/
	function time(?label:String):Void;

	/**
		Stops a timer that was previously started by calling console.time() and prints the result to `stdout`:

		@see https://nodejs.org/api/console.html#console_console_timeend_label
	**/
	function timeEnd(?label:String):Void;

	/**
		For a timer that was previously started by calling console.time(), prints the elapsed time and other `data` arguments to `stdout`:

		@see https://nodejs.org/api/console.html#console_console_timelog_label_data
	**/
	function timeLog(?label:String, data:Rest<Dynamic>):Void;

	/**
		Prints to `stderr` the string `'Trace: '`, followed by the util.format() formatted message and stack trace to the current position in the code.

		@see https://nodejs.org/api/console.html#console_console_trace_message_args
	**/
	function trace(message:Dynamic, args:Rest<Dynamic>):Void;

	/**
		The `console.warn()` function is an alias for console.error().

		@see https://nodejs.org/api/console.html#console_console_warn_data_args
	**/
	function warn(data:Dynamic, args:Rest<Dynamic>):Void;

	/**
		This method does not display anything unless used in the inspector. The `console.markTimeline()` method is the deprecated form of console.timeStamp().

		@see https://nodejs.org/api/console.html#console_console_marktimeline_label
	**/
	function markTimeline(?label:String):Void;

	/**
		This method does not display anything unless used in the inspector.
		The `console.profile()` method starts a JavaScript CPU profile with an optional label until console.profileEnd() is called.
		The profile is then added to the Profile panel of the inspector.

		@see https://nodejs.org/api/console.html#console_console_profile_label
	**/
	function profile(?label:String):Void;

	/**
		This method does not display anything unless used in the inspector.
		Stops the current JavaScript CPU profiling session if one has been started and prints the report to the Profiles panel of the inspector.
		See console.profile() for an example.

		@see https://nodejs.org/api/console.html#console_console_profileend_label
	**/
	function profileEnd(?label:String):Void;

	/**
		This method does not display anything unless used in the inspector.
		The `console.timeStamp()` method adds an event with the label `'label'` to the Timeline panel of the inspector.

		@see https://nodejs.org/api/console.html#console_console_timestamp_label
	**/
	function timeStamp(?label:String):Void;

	/**
		This method does not display anything unless used in the inspector. The `console.timeline()` method is the deprecated form of console.time().

		@see https://nodejs.org/api/console.html#console_console_timeline_label
	**/
	function timeline(?label:String):Void;

	/**
		This method does not display anything unless used in the inspector. The `console.timelineEnd()` method is the deprecated form of console.timeEnd().

		@see https://nodejs.org/api/console.html#console_console_timelineend_label
	**/
	function timelineEnd(?label:String):Void;
}

typedef ConsoleOptions = {
	/**
		`stdout` is a writable stream to print log or info output.
	**/
	var stdout:IWritable;

	/**
		`stderr` is used for warning or error output. If stderr is not provided, stdout is used for stderr.
	**/
	@optional var stderr:IWritable;

	/**
		Ignore errors when writing to the underlying streams. Default: `true`.
	**/
	@optional var ignoreErrors:Bool;

	/**
		Set color support for this `Console` instance. Setting to `true` enables coloring while inspecting values,
		setting to `'auto'` will make color support depend on the value of the `isTTY` property and the value returned by `getColorDepth()` on the respective stream.
		 This option can not be used, if `inspectOptions.colors` is set as well. Default: `'auto'`.
	**/
	@optional var colorMode:EitherType<Bool, String>;

	/**
		Specifies options that are passed along to util.inspect().
	**/
	@optional var inspectOptions:Util.InspectOptions;
}
