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
import hl.types.ArrayObj;
import hl.types.BytesMap;
import hl.NativeArray;

/**
	Predefined file descriptors for stdio.
**/
enum abstract StdioFd(Int) from Int to Int {
	var STDIN = 0;
	var STDOUT = 1;
	var STDERR = 3;
}

/**
	Containers for each stdio handle or fd passed to a child process.
**/
enum ProcessStdio {
	/**
		Redirect child process descriptor to `/dev/null`(Unix) or `NULL`(Windows)
	**/
	IGNORE;
	/**
		Connect child process descriptior to the corresponding parent process descriptor.
	**/
	INHERIT;
	/**
		Connect child process descriptor to the specified file descriptor.

		Predefined descriptors from `StdioFd` are allowed as well as any other
		file descriptor represented by an `Int` value.
	**/
	FD(fd:StdioFd);
	/**
		Connect child proces descriptor to the specified pipe.

		Specifying `nonBlock` opens the handle in non-blocking mode in the child.
		This may cause loss of data, if the child is not designed to handle to
		encounter this mode, but can also be significantly more efficient.
	**/
	PIPE(pipe:Pipe, permissions:StdioPipePermissions, ?nonBlock:Bool);
	/**
		Connect child proces descriptor to the specified stream.
	**/
	STREAM(stream:Stream);
}

/**
	Determine the direction of flow from the child process' perspective.
**/
enum abstract StdioPipePermissions(Int) {
	var READ = 1;
	var WRITE = 2;
	var DUPLEX = 3;
}

/**
	Options for spawning processes.
**/
typedef ProcessOptions = {
	/** Callback called after the process exits. */
	var ?onExit:(p:Process, exitStatus:I64, termSignal:SigNum)->Void;
	/**
		Describes the file descriptors that will be made available to the child process.
		The convention is that stdio[0] points to stdin, stdio[1] is used for stdout,
		and stdio[2] is stderr.

		On Windows file descriptors greater than 2 are available to the child process
		only if the child processes uses the MSVCRT runtime.

		By default uses `ProcessStdio.IGNORE` for missing descriptors.
	**/
	var ?stdio:Array<ProcessStdio>;
	/** Environment for the new process. The parents environment is used by default. */
	var ?env:Map<String,String>;
	/** Current working directory for the subprocess. */
	var ?cwd:String;
	/** Set user id of the child process to this value. This is not supported on Windows. */
	var ?uid:Int;
	/** Set group id of the child process to this value. This is not supported on Windows. */
	var ?gid:Int;
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
	var ?windowsHideGui:Bool;
}

/**
	Process handles will spawn a new process and allow the user to control it
	and establish communication channels with it using streams.

	@see http://docs.libuv.org/en/v1.x/process.html
**/
@:forward
abstract Process(Handle) to Handle {
	/** The PID of the spawned process. It’s set after calling `spawn()`. */
	public var pid(get,never):Int;
	@:hlNative("uv", "process_pid") public function get_pid():Int return 0;

	/**
		Disables (tries) file descriptor inheritance for inherited descriptors.

		The effect is that child processes spawned by this process don’t accidentally
		inherit these handles.
	**/
	@:hlNative("uv", "disable_stdio_inheritance")
	static public function disableStdioInheritance():Void {}

	/**
		Initializes the process handle and starts the process.

		`cmd` is the program to be executed.
		`args[0]` should be the path to the program.
	**/
	static public function spawn(loop:Loop, cmd:String, args:Array<String>, ?options:ProcessOptions):Process {
		inline function toNative<T>(array:Array<T>):NativeArray<T> {
			var result = new NativeArray<T>(array.length);
			for(i => v in array)
				result[i] = v;
			return result;
		}
		if(options == null) {
			return spawn_wrap(loop, cmd, toNative(args));
		} else {
			var stdio = null;
			if(options.stdio != null)
				stdio = toNative(options.stdio);
			var env = null;
			if(options.env != null){
				var envArray = [for(k => v in options.env) '$k=$v'];
				env = toNative(envArray);
			}
			return spawn_wrap(loop, cmd, toNative(args), options.onExit, stdio, env, options.cwd,
				options.uid, options.gid, options.detached, options.windowsVerbatimArguments,
				options.windowsHide, options.windowsHideConsole, options.windowsHideGui);
		}
	}

	@:hlNative("uv", "spawn_wrap") static function spawn_wrap(
		loop:Loop,
		file:String,
		args:NativeArray<String>,
		?onExti:(p:Process, exitStatus:I64, termSignal:Int)->Void,
		?stdio:NativeArray<ProcessStdio>,
		?env:NativeArray<String>,
		?cwd:String,
		?uid:Int,
		?gid:Int,
		?detached:Bool,
		?windowsVerbatimArguments:Bool,
		?windowsHide:Bool,
		?windowsHideConsole:Bool,
		?windowsHideGui:Bool
	):Process
		return null;


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