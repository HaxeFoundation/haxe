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

package js.node;

import haxe.DynamicAccess;
import haxe.extern.EitherType;
import haxe.extern.Rest;
import js.node.child_process.ChildProcess.ChildProcessSendOptions;
import js.node.events.EventEmitter;
import js.node.stream.Readable;
import js.node.stream.Writable;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by the Process class.
**/
enum abstract ProcessEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when the process is about to exit.
		There is no way to prevent the exiting of the event loop at this point,
		and once all exit listeners have finished running the process will exit.
		Therefore you must only perform synchronous operations in this handler.
		This is a good hook to perform checks on the module's state (like for unit tests).
		The callback takes one argument, the code the process is exiting with.
	**/
	var Exit:ProcessEvent<Int->Void> = "exit";

	/**
		Emitted when node empties it's event loop and has nothing else to schedule.

		Normally, node exits when there is no work scheduled, but a listener for `beforeExit`
		can make asynchronous calls, and cause node to continue.

		`beforeExit` is not emitted for conditions causing explicit termination, such as `process.exit()`
		or uncaught exceptions, and should not be used as an alternative to the `exit` event
		unless the intention is to schedule more work.
	**/
	var BeforeExit:ProcessEvent<Int->Void> = "beforeExit";

	/**
		Emitted when an exception bubbles all the way back to the event loop.
		If a listener is added for this exception, the default action (which is to print a stack trace and exit)
		will not occur.
	**/
	var UncaughtException:ProcessEvent<Error->Void> = "uncaughtException";
}

extern class Process extends EventEmitter<Process> {
	/**
		A Writable Stream to stdout.

		`stderr` and `stdout` are unlike other streams in Node in that writes to them are usually blocking.
	**/
	var stdout:IWritable;

	/**
		A writable stream to stderr.

		`stderr` and `stdout` are unlike other streams in Node in that writes to them are usually blocking.
	**/
	var stderr:IWritable;

	/**
		A Readable Stream for stdin.
	**/
	var stdin:IReadable;

	/**
		An array containing the command line arguments.
		The first element will be `node`, the second element will be the name of the JavaScript file.
		The next elements will be any additional command line arguments.

		E.g:
			$ node process-2.js one two=three four
			0: node
			1: /Users/mjr/work/node/process-2.js
			2: one
			3: two=three
			4: four
	**/
	var argv:Array<String>;

	/**
		This is the absolute pathname of the executable that started the process.
	**/
	var execPath:String;

	/**
		This is the set of node-specific command line options from the executable that started the process.
		These options do not show up in `argv`, and do not include the node executable, the name of the script,
		or any options following the script name.

		These options are useful in order to spawn child processes with the same execution environment as the parent.
	**/
	var execArgv:Array<String>;

	/**
		This causes node to emit an abort. This will cause node to exit and generate a core file.
	**/
	function abort():Void;

	/**
		Changes the current working directory of the process or throws an exception if that fails.
	**/
	function chdir(directory:String):Void;

	/**
		Returns the current working directory of the process.
	**/
	function cwd():String;

	/**
		An object containing the user environment. See environ(7).
	**/
	var env:DynamicAccess<String>;

	/**
		Ends the process with the specified `code`. If the `code` is omitted, exit uses either the
		'success' code `0` or the value of `process.exitCode` if specified.
	**/
	function exit(?code:Int):Void;

	/**
		A number which will be the process exit code, when the process either exits gracefully,
		or is exited via `process.exit()` without specifying a code.

		Specifying a code to `process.exit(code)` will override any previous setting of `process.exitCode`.
	**/
	var exitCode:Null<Int>;

	/**
		Gets the group identity of the process. See getgid(2).
		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	function getgid():Int;

	/**
		Sets the group identity of the process. See setgid(2).
		This accepts either a numerical ID or a groupname string.
		If a groupname is specified, this method blocks while resolving it to a numerical ID.

		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	@:overload(function(id:String):Void {})
	function setgid(id:Int):Void;

	/**
		Gets the user identity of the process. See getuid(2).
		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	function getuid():Int;

	/**
		Sets the user identity of the process. See setuid(2).
		This accepts either a numerical ID or a username string.
		If a username is specified, this method blocks while resolving it to a numerical ID.

		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	@:overload(function(id:String):Void {})
	function setuid(id:Int):Void;

	/**
		Returns an array with the supplementary group IDs.
		POSIX leaves it unspecified if the effective group ID is included but node.js ensures it always is.
		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	function getgroups():Array<Int>;

	/**
		Sets the supplementary group IDs.
		This is a privileged operation, meaning you need to be root or have the CAP_SETGID capability.

		Note: this function is only available on POSIX platforms (i.e. not Windows)
		The list can contain group IDs, group names or both.
	**/
	function setgroups(groups:Array<EitherType<String, Int>>):Void;

	/**
		Reads /etc/group and initializes the group access list, using all groups of which the user is a member.
		This is a privileged operation, meaning you need to be root or have the CAP_SETGID capability.

		Note: this function is only available on POSIX platforms (i.e. not Windows)
	**/
	function initgroups(user:EitherType<String, Int>, extra_group:EitherType<String, Int>):Void;

	/**
		A compiled-in property that exposes NODE_VERSION.
	**/
	var version(default, null):String;

	/**
		A property exposing version strings of node and its dependencies.
	**/
	var versions:DynamicAccess<String>;

	/**
		An Object containing the JavaScript representation of the configure options that were used to compile the current node executable.
		This is the same as the "config.gypi" file that was produced when running the ./configure script.
	**/
	var config:Dynamic<Dynamic>;

	/**
		Send a signal to a process.
		`pid` is the process id and `signal` is the string describing the signal to send. Signal names are strings like 'SIGINT' or 'SIGHUP'.

		If omitted, the `signal` will be 'SIGTERM'. See Signal Events and kill(2) for more information.

		Will throw an error if target does not exist, and as a special case,
		a signal of 0 can be used to test for the existence of a process.

		Note that just because the name of this function is `kill`, it is really just a signal sender, like the kill system call.
		The signal sent may do something other than kill the target process.
	**/
	function kill(pid:Int, ?signal:String):Void;

	/**
		The PID of the process.
	**/
	var pid(default, null):Int;

	/**
		Getter/setter to set what is displayed in 'ps'.

		When used as a setter, the maximum length is platform-specific and probably short.
		On Linux and OS X, it's limited to the size of the binary name plus the length of the
		command line arguments because it overwrites the argv memory.
	**/
	var title:String;

	/**
		What processor architecture you're running on: 'arm', 'ia32', or 'x64'.
	**/
	var arch:String;

	/**
		What platform you're running on: 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
	**/
	var platform:String;

	/**
		The PID of the parent process
	**/
	var ppid:Int;

	/**
		The metadata of the current release
	**/
	var release:Release;

	/**
		Used for diagnostic reports
	**/
	var report:Report;

	/**
		Returns an object describing the memory usage of the Node process measured in bytes.
	**/
	function memoryUsage():MemoryUsage;

	/**
		On the next loop around the event loop call this callback.
		This is not a simple alias to setTimeout(fn, 0), it's much more efficient.
		It typically runs before any other I/O events fire, but there are some exceptions.

		This is important in developing APIs where you want to give the user the chance to
		assign event handlers after an object has been constructed, but before any I/O has occurred.
	**/
	function nextTick(callback:Void->Void, args:Rest<Dynamic>):Void;

	/**
		Sets or reads the process's file mode creation mask.
		Child processes inherit the mask from the parent process.
		Returns the old mask if mask argument is given, otherwise returns the current mask.
	**/
	function umask(?mask:Int):Int;

	/**
		Number of seconds Node has been running.
	**/
	function uptime():Float;

	/**
		Returns the current high-resolution real time in a [seconds, nanoseconds] tuple Array.
		It is relative to an arbitrary time in the past.
		It is not related to the time of day and therefore not subject to clock drift.
		The primary use is for measuring performance between intervals.
		You may pass in the result of a previous call to `hrtime` to get a diff reading,
		useful for benchmarks and measuring intervals
	**/
	@:overload(function(prev:Array<Float>):Array<Float> {})
	function hrtime():Array<Float>;

	/**
		Alternate way to retrieve require.main. The difference is that if the main module changes at runtime,
		require.main might still refer to the original main module in modules that were required
		before the change occurred. Generally it's safe to assume that the two refer to the same module.

		As with require.main, it will be undefined if there was no entry script.
	**/
	var mainModule(default, null):Module;

	/**
		Send a message to the parent process.

		Only available for child processes. See `ChildProcess.send`.
	**/
	@:overload(function(message:Dynamic, sendHandle:Dynamic, options:ChildProcessSendOptions, ?callback:Error->Void):Bool {})
	@:overload(function(message:Dynamic, sendHandle:Dynamic, ?callback:Error->Void):Bool {})
	function send(message:Dynamic, ?callback:Error->Void):Bool;

	/**
		Close the IPC channel to parent process.

		Only available for child processes. See `ChildProcess.disconnect`.
	**/
	function disconnect():Void;

	/**
		Disable run-time deprecation warnings.
		See `Util.deprecate`.
	**/
	var noDeprecation:Bool;

	/**
		Enable logging of deprecation warnings.
		See `Util.deprecate`.
	**/
	var traceDeprecation:Bool;

	/**
		Throw on deprecated API usage.
		See `Util.deprecate`.
	**/
	var throwDeprecation:Bool;
}

typedef MemoryUsage = {
	rss:Float,
	heapTotal:Float,
	heapUsed:Float
}

typedef Release = {
	name:String,
	?sourceUrl:String,
	?headersUrl:String,
	?libUrl:String,
	?lts:String
}
