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
import js.node.child_process.ChildProcess as ChildProcessObject;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Common options for all `ChildProcess` methods.
**/
private typedef ChildProcessCommonOptions = {
	/**
		Current working directory of the child process.
	**/
	@:optional var cwd:String;

	/**
		Environment key-value pairs
	**/
	@:optional var env:DynamicAccess<String>;

	/**
		Sets the user identity of the process. See setuid(2).
	**/
	@:optional var uid:Int;

	/**
		Sets the group identity of the process. See setgid(2).
	**/
	@:optional var gid:Int;

	/**
		Shell to execute the command with.
		Default: '/bin/sh' on UNIX, 'cmd.exe' on Windows.

		The shell should understand the -c switch on UNIX or /s /c on Windows.
		On Windows, command line parsing should be compatible with cmd.exe.
	**/
	@:optional var shell:EitherType<Bool, String>;
}

/**
	Common options for `spawn` and `spawnSync` methods.
**/
private typedef ChildProcessSpawnOptionsBase = {
	> ChildProcessCommonOptions,

	/**
		Child's stdio configuration.
	**/
	@:optional var stdio:ChildProcessSpawnOptionsStdio;
}

/**
	Options for the `spawn` method.
**/
typedef ChildProcessSpawnOptions = {
	> ChildProcessSpawnOptionsBase,

	/**
		The child will be a process group leader.
	**/
	@:optional var detached:Bool;
}

/**
	Options for the `spawnSync` method.
**/
typedef ChildProcessSpawnSyncOptions = {
	> ChildProcessSpawnOptionsBase,
	> ChildProcessExecOptionsBase,

	@:optional var input:EitherType<String, Buffer>;
}

/**
	The `stdio` option is an array where each index corresponds to a fd in the child.
	The value is one of the following:

		* 'pipe' - Create a pipe between the child process and the parent process.
				   The parent end of the pipe is exposed to the parent as a property on the child_process object as ChildProcess.stdio[fd].
				   Pipes created for fds 0 - 2 are also available as ChildProcess.stdin, ChildProcess.stdout and ChildProcess.stderr, respectively.

		* 'ipc' - Create an IPC channel for passing messages/file descriptors between parent and child.
				  A ChildProcess may have at most one IPC stdio file descriptor. Setting this option enables the ChildProcess.send() method.
				  If the child writes JSON messages to this file descriptor, then this will trigger ChildProcess.on('message').
				  If the child is a Node.js program, then the presence of an IPC channel will enable process.send() and process.on('message').

		* 'ignore' - Do not set this file descriptor in the child. Note that Node will always open fd 0 - 2 for the processes it spawns.
					 When any of these is ignored node will open /dev/null and attach it to the child's fd.

		* Stream object - Share a readable or writable stream that refers to a tty, file, socket, or a pipe with the child process.
						  The stream's underlying file descriptor is duplicated in the child process to the fd that corresponds to the index
						  in the stdio array. Note that the stream must have an underlying descriptor (file streams do not until the 'open'
						  event has occurred).

		* Positive integer - The integer value is interpreted as a file descriptor that is is currently open in the parent process.
							 It is shared with the child process, similar to how Stream objects can be shared.

		* null - Use default value. For stdio fds 0, 1 and 2 (in other words, stdin, stdout, and stderr) a pipe is created.
				 For fd 3 and up, the default is 'ignore'.

	 As a shorthand, the stdio argument may also be one of the following strings, rather than an array:
		ignore - ['ignore', 'ignore', 'ignore']
		pipe - ['pipe', 'pipe', 'pipe']
		inherit - [process.stdin, process.stdout, process.stderr] or [0,1,2]
**/
typedef ChildProcessSpawnOptionsStdio = EitherType<ChildProcessSpawnOptionsStdioSimple, ChildProcessSpawnOptionsStdioFull>;

/**
	A shorthand for the `stdio` argument in `ChildProcessSpawnOptions`
**/
enum abstract ChildProcessSpawnOptionsStdioSimple(String) from String to String {
	/**
		Equivalent to ['ignore', 'ignore', 'ignore']
	**/
	var Ignore = "ignore";

	/**
		Equivalent to ['pipe', 'pipe', 'pipe']
	**/
	var Pipe = "pipe";

	/**
		Equivalent to [process.stdin, process.stdout, process.stderr] or [0,1,2]
	**/
	var Inherit = "inherit";
}

/**
	Enumeration of possible `stdio` behaviours.
**/
enum abstract ChildProcessSpawnOptionsStdioBehaviour(String) from String to String {
	/**
		Create a pipe between the child process and the parent process.
		The parent end of the pipe is exposed to the parent as a property on the child_process object as ChildProcess.stdio[fd].
		Pipes created for fds 0 - 2 are also available as ChildProcess.stdin, ChildProcess.stdout and ChildProcess.stderr, respectively.
	**/
	var Pipe = "pipe";

	/**
		Create an IPC channel for passing messages/file descriptors between parent and child.
		A ChildProcess may have at most one IPC stdio file descriptor.

		Setting this option enables the ChildProcess.send() method.

		If the child writes JSON messages to this file descriptor, then this will trigger
		ChildProcess.on('message').

		If the child is a Node.js program, then the presence of an IPC channel will
		enable process.send() and process.on('message').
	**/
	var Ipc = "ipc";

	/**
		Do not set this file descriptor in the child.
		Note that Node will always open fd 0 - 2 for the processes it spawns.
		When any of these is ignored node will open /dev/null and attach it to the child's fd.
	**/
	var Ignore = "ignore";
}

// see https://github.com/HaxeFoundation/haxe/issues/3499
// typedef ChildProcessSpawnOptionsStdioFull = Array<EitherType<ChildProcessSpawnOptionsStdioBehaviour,EitherType<IStream,Int>>>;
typedef ChildProcessSpawnOptionsStdioFull = Array<Dynamic>;

/**
	Common options for `exec` and `execFile` methods.
**/
private typedef ChildProcessExecOptionsBase = {
	> ChildProcessCommonOptions,

	/**
		Default: 'utf8'
	**/
	@:optional var encoding:String;

	/**
		If greater than 0, then it will kill the child process if it runs longer than timeout milliseconds.
	**/
	@:optional var timeout:Int;

	/**
		The child process is killed with `killSignal` (default: 'SIGTERM').
	**/
	@:optional var killSignal:String;

	/**
		The largest amount of data allowed on stdout or stderr.
		If this value is exceeded then the child process is killed.
		Default: 200*1024
	**/
	@:optional var maxBuffer:Int;
}

/**
	Options for the `exec` method.
**/
typedef ChildProcessExecOptions = {
	> ChildProcessExecOptionsBase,
}

/**
	Options for the `execFile` method.
**/
typedef ChildProcessExecFileOptions = {
	> ChildProcessExecOptionsBase,
}

/**
	Options for the `fork` method.
**/
typedef ChildProcessForkOptions = {
	> ChildProcessCommonOptions,

	/**
		Executable used to create the child process
	**/
	@:optional var execPath:String;

	/**
		List of string arguments passed to the executable (Default: process.execArgv)
	**/
	@:optional var execArgv:Array<String>;

	/**
		If `true`, stdin, stdout, and stderr of the child will be piped to the parent,
		otherwise they will be inherited from the parent, see the "pipe" and "inherit"
		options for `ChildProcessSpawnOptions.stdio` for more details (default is `false`)
	**/
	@:optional var silent:Bool;
}

/**
	An error passed to the `ChildProcess.exec` callback.
**/
@:native("Error")
extern class ChildProcessExecError extends Error {
	/**
		the exit code of the child proces.
	**/
	var code(default, null):Int;

	/**
		the signal that terminated the process.
	**/
	var signal(default, null):String;
}

/**
	A callback type for `ChildProcess.exec`.
	It receives three arguments: `error`, `stdout`, `stderr`.

	On success, error will be `null`. On error, `error` will be an instance of `Error`
	and `error.code` will be the exit code of the child process, and `error.signal` will be set
	to the signal that terminated the process (see `ChildProcessExecError`).
**/
typedef ChildProcessExecCallback = #if (haxe_ver >= 4) (error:Null<ChildProcessExecError>, stdout:EitherType<Buffer, String>,
		stderr:EitherType<Buffer, String>) -> Void; #else Null<ChildProcessExecError>->EitherType<Buffer, String>->EitherType<Buffer, String>->Void; #end

/**
	Object returned from the `spawnSync` method.
**/
typedef ChildProcessSpawnSyncResult = {
	/**
		Pid of the child process
	**/
	var pid:Int;

	/**
		Array of results from stdio output
	**/
	var output:Array<EitherType<Buffer, String>>;

	/**
		The contents of output[1]
	**/
	var stdout:EitherType<Buffer, String>;

	/**
		The contents of output[2]
	**/
	var stderr:EitherType<Buffer, String>;

	/**
		The exit code of the child process
	**/
	var status:Int;

	/**
		The signal used to kill the child process
	**/
	var signal:String;

	/**
		The error object if the child process failed or timed out
	**/
	var error:Error;
}

@:jsRequire("child_process")
extern class ChildProcess {
	/**
		Launches a new process with the given `command`, with command line arguments in `args`.
		If omitted, `args` defaults to an empty `Array`.

		The third argument is used to specify additional options, which defaults to:
			{ cwd: null,
			  env: process.env
			}

		Note that if spawn receives an empty options object, it will result in spawning the process with an empty
		environment rather than using `process.env`. This due to backwards compatibility issues with a deprecated API.
	**/
	@:overload(function(command:String, ?options:ChildProcessSpawnOptions):ChildProcessObject {})
	@:overload(function(command:String, args:Array<String>, ?options:ChildProcessSpawnOptions):ChildProcessObject {})
	static function spawn(command:String, ?args:Array<String>):ChildProcessObject;

	/**
		Runs a command in a shell and buffers the output.

		`command` is the command to run, with space-separated arguments.

		The default `options` are:
			{ encoding: 'utf8',
			  timeout: 0,
			  maxBuffer: 200*1024,
			  killSignal: 'SIGTERM',
			  cwd: null,
			  env: null }
	**/
	@:overload(function(command:String, options:ChildProcessExecOptions, callback:ChildProcessExecCallback):ChildProcessObject {})
	static function exec(command:String, callback:ChildProcessExecCallback):ChildProcessObject;

	/**
		This is similar to `exec` except it does not execute a subshell but rather the specified file directly.
		This makes it slightly leaner than `exec`
	**/
	@:overload(function(file:String, args:Array<String>, options:ChildProcessExecFileOptions, ?callback:ChildProcessExecCallback):ChildProcessObject {})
	@:overload(function(file:String, options:ChildProcessExecFileOptions, ?callback:ChildProcessExecCallback):ChildProcessObject {})
	@:overload(function(file:String, args:Array<String>, ?callback:ChildProcessExecCallback):ChildProcessObject {})
	static function execFile(file:String, ?callback:ChildProcessExecCallback):ChildProcessObject;

	/**
		This is a special case of the `spawn` functionality for spawning Node processes.
		In addition to having all the methods in a normal `ChildProcess` instance,
		the returned object has a communication channel built-in.
		See `send` for details.
	**/
	@:overload(function(modulePath:String, args:Array<String>, options:ChildProcessForkOptions):ChildProcessObject {})
	@:overload(function(modulePath:String, options:ChildProcessForkOptions):ChildProcessObject {})
	static function fork(modulePath:String, ?args:Array<String>):ChildProcessObject;

	/**
		Synchronous version of `spawn`.

		`spawnSync` will not return until the child process has fully closed.
		When a timeout has been encountered and `killSignal` is sent, the method won't return until the process
		has completely exited. That is to say, if the process handles the SIGTERM signal and doesn't exit,
		your process will wait until the child process has exited.
	**/
	@:overload(function(command:String, args:Array<String>, ?options:ChildProcessSpawnSyncOptions):ChildProcessSpawnSyncResult {})
	static function spawnSync(command:String, ?options:ChildProcessSpawnSyncOptions):ChildProcessSpawnSyncResult;

	/**
		Synchronous version of `execFile`.

		`execFileSync` will not return until the child process has fully closed.
		When a timeout has been encountered and `killSignal` is sent, the method won't return until the process
		has completely exited. That is to say, if the process handles the SIGTERM signal and doesn't exit,
		your process will wait until the child process has exited.

		If the process times out, or has a non-zero exit code, this method will throw.
		The Error object will contain the entire result from `spawnSync`
	**/
	@:overload(function(command:String, ?options:ChildProcessSpawnSyncOptions):EitherType<String, Buffer> {})
	@:overload(function(command:String, args:Array<String>, ?options:ChildProcessSpawnSyncOptions):EitherType<String, Buffer> {})
	static function execFileSync(command:String, ?args:Array<String>):EitherType<String, Buffer>;

	/**
		Synchronous version of `exec`.

		`execSync` will not return until the child process has fully closed.
		When a timeout has been encountered and `killSignal` is sent, the method won't return until the process
		has completely exited. That is to say, if the process handles the SIGTERM signal and doesn't exit,
		your process will wait until the child process has exited.

		If the process times out, or has a non-zero exit code, this method will throw.
		The Error object will contain the entire result from `spawnSync`
	**/
	static function execSync(command:String, ?options:ChildProcessSpawnSyncOptions):EitherType<String, Buffer>;
}
