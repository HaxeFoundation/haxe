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

package hl.uv;

import hl.uv.Signal.SigNum;

/**
	Container for each stdio handle or fd passed to a child process.
**/
typedef ProcessStdio = {
	var ?ignore:Bool;
	var ?fd:Int;
	var ?stream:Stream;
	var ?createPipe:Bool;
	/**
		When var `createPipe` is specified, `readablePipe` and `writablePipe` determine
		the direction of flow, from the child process' perspective. Both flags may be
		specified to create a duplex data stream.
	**/
	var ?readablePipe:Bool;
	var ?writablePipe:Bool;
	/**
		When `createPipe` is specified, specifying `nonBlockPipe` opens the handle
		in non-blocking mode in the child. This may cause loss of data, if the child
		is not designed to handle to encounter this mode, but can also be significantly
		more efficient.
	**/
	var ?nonBlockPipe:Bool;
}

/**
	Options for spawning processes.
**/
typedef ProcessOptions = {
	/** Path pointing to the program to be executed. */
	var file:String;
	/** Command line arguments. args[0] should be the path to the program. */
	var args:Array<String>;
	/** Callback called after the process exits. */
	var ?onExit:(p:Process, exitStatus:I64, termSignal:Int)->Void;
	/**
		Describes the file descriptors that will be made available to the child process.
		The convention is that stdio[0] points to stdin, stdio[1] is used for stdout,
		and stdio[2] is stderr.
		On Windows file descriptors greater than 2 are available to the child process
		only if the child processes uses the MSVCRT runtime.
	**/
	var ?stdio:Array<ProcessStdio>;
	/** Environment for the new process. The parents environment is used by default. */
	var ?env:Map<String,String>;
	/** Current working directory for the subprocess. */
	var ?cwd:String;
	/** Set user id of the child process to this value. This is not supported on Windows. */
	var ?uid:Int;
	/** Set group id of the child process to this value. This is not supported on Windows. */
	var ?git:Int;
	/**
		Spawn the child process in a detached state - this will make it a process
		group leader, and will effectively enable the child to keep running after
		the parent exits.
	**/
	var ?detached:Bool;
	/**
		Do not wrap any arguments in quotes, or perform any other escaping, when
		converting the argument list into a command line string. This option is
		only meaningful on Windows systems. On Unix it is silently ignored.
	**/
	var ?windowsVerbatimArguments:Bool;
	/** Hide the subprocess window that would normally be created. Windows only. */
	var ?windowsHide:Bool;
	/** Hide the subprocess console window that would normally be created. Windows only. */
	var ?windowsHideConsole:Bool;
	/** Hide the subprocess GUI window that would normally be created. Windows only. */
	var ?windowsHideGuiWindow:Bool;
}

/**
	Process handles will spawn a new process and allow the user to control it
	and establish communication channels with it using streams.

	@see http://docs.libuv.org/en/v1.x/process.html
**/
@:forward
abstract Process(HandleData) to HandleData {

	/** The PID of the spawned process. It’s set after calling `spawn()`. */
	public var pid(get,never):Int;
	@:hlNative("uv", "process_pid") public function get_sigNum():Int return 0;

	/**
		Disables (tries) file descriptor inheritance for inherited descriptors.
	**/
	@:hlNative("uv", "disable_stdio_inheritance")
	static public function disableStdioInheritance():Void {}

	/**
		Initializes the process handle and starts the process.
	**/
	@:hlNative("uv", "spawn_wrap")
	public function spawn(loop:Loop, options:ProcessOptions):Void {}

	/**
		Sends the specified signal to the process.
	**/
	@:hlNative("uv", "process_kill_wrap")
	public function kill(sigNum:SigNum):Void {}

	/**
		Sends the specified signal to the process with the specified pid.
	**/
	@:hlNative("uv", "kill_wrap")
	static public function killPid(pid:Int, sigNum:SigNum):Void {}

}