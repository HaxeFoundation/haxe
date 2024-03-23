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

import hl.types.ArrayBase;
import hl.uv.Signal.SigNum;
import hl.types.ArrayObj;
import hl.types.BytesMap;
import hl.NativeArray;

using hl.uv.UV;

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
	**/
	FD(fd:File);
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
	STREAM<T:UvStreamTStar>(stream:Stream<T>);
}

/**
	Determine the direction of flow from the child process' perspective.
**/
enum abstract StdioPipePermissions(Int) to Int {
	var READ = UV_READABLE_PIPE;
	var WRITE = UV_WRITABLE_PIPE;
	var DUPLEX = UV_READABLE_PIPE | UV_WRITABLE_PIPE;
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
	Flags specifying how a stdio should be transmitted to the child process.
**/
enum abstract StdioFlag(Int) to Int {
	var UV_IGNORE = 0x00;
	var UV_CREATE_PIPE = 0x01;
	var UV_INHERIT_FD = 0x02;
	var UV_INHERIT_STREAM = 0x04;
	/**
		When UV_CREATE_PIPE is specified, UV_READABLE_PIPE and UV_WRITABLE_PIPE
		determine the direction of flow, from the child process' perspective. Both
		flags may be specified to create a duplex data stream.
	**/
	var UV_READABLE_PIPE = 0x10;
	var UV_WRITABLE_PIPE = 0x20;
	/**
		When UV_CREATE_PIPE is specified, specifying UV_NONBLOCK_PIPE opens the
		handle in non-blocking mode in the child. This may cause loss of data,
		if the child is not designed to handle to encounter this mode,
		but can also be significantly more efficient.
	**/
	var UV_NONBLOCK_PIPE = 0x40;
}

/*
 * These are the flags that can be used for the uv_process_options.flags field.
 */
enum abstract ProcessFlag(Int) to Int {
	/**
		Set the child process' user id. The user id is supplied in the `uid` field
		of the options struct. This does not work on windows; setting this flag
		will cause uv_spawn() to fail.
	**/
	var UV_PROCESS_SETUID = 1 << 0;
	/**
		Set the child process' group id. The user id is supplied in the `gid`
		field of the options struct. This does not work on windows; setting this
		flag will cause uv_spawn() to fail.
	**/
	var UV_PROCESS_SETGID = 1 << 1;
	/**
		Do not wrap any arguments in quotes, or perform any other escaping, when
		converting the argument list into a command line string. This option is
		only meaningful on Windows systems. On Unix it is silently ignored.
	**/
	var UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS = 1 << 2;
	/**
		Spawn the child process in a detached state - this will make it a process
		group leader, and will effectively enable the child to keep running after
		the parent exits.  Note that the child process will still keep the
		parent's event loop alive unless the parent process calls uv_unref() on
		the child's process handle.
	**/
	var UV_PROCESS_DETACHED = 1 << 3;
	/**
		Hide the subprocess window that would normally be created. This option is
		only meaningful on Windows systems. On Unix it is silently ignored.
	**/
	var UV_PROCESS_WINDOWS_HIDE = 1 << 4;
	/**
		Hide the subprocess console window that would normally be created. This
		option is only meaningful on Windows systems. On Unix it is silently
		ignored.
	**/
	var UV_PROCESS_WINDOWS_HIDE_CONSOLE = 1 << 5;
	/**
		Hide the subprocess GUI window that would normally be created. This
		option is only meaningful on Windows systems. On Unix it is silently
		ignored.
	**/
	var UV_PROCESS_WINDOWS_HIDE_GUI = 1 << 6;
}

/**
	Process handles will spawn a new process and allow the user to control it
	and establish communication channels with it using streams.

	@see http://docs.libuv.org/en/v1.x/process.html
**/
class Process extends Handle<UvProcessTStar> {
	@:keep var onExit:(p:Process, exitStatus:I64, termSignal:SigNum)->Void;

	/** The PID of the spawned process. It’s set after calling `spawn()`. */
	public var pid(get,never):Int;
	function get_pid():Int {
		return handleReturn(h -> h.process_get_pid());
	}

	/**
		Disables (tries) file descriptor inheritance for inherited descriptors.

		The effect is that child processes spawned by this process don’t accidentally
		inherit these handles.
	**/
	static public inline function disableStdioInheritance():Void {
		UV.disable_stdio_inheritance();
	}

	/**
		Initializes the process handle and starts the process.

		`cmd` is the program to be executed.
		`args[0]` should be the path to the program.
	**/
	static public function spawn(loop:Loop, cmd:String, args:Array<String>, ?options:ProcessOptions):Process {
		loop.checkLoop();

		var cArgs = UV.alloc_char_array(args.length + 1);
		cArgs.offset(args.length).set(null);
		for(i => a in args)
			cArgs.offset(i).set(a.toUTF8());

		var env = null;
		var cwd = null;
		var flags = 0;
		var stdioCount = 0;
		var stdio = null;
		var uid = 0;
		var gid = 0;
		if(options != null) {
			if(options.stdio != null) {
				stdioCount = options.stdio.length;
				stdio = UV.alloc_stdio_container(@:privateAccess (cast options.stdio:ArrayObj<Dynamic>).array, stdioCount);
			}
			if( options.cwd != null )
				cwd = options.cwd.toUTF8();
			if(options.uid != null) {
				uid = options.uid;
				flags |= UV_PROCESS_SETUID;
			}
			if(options.gid != null) {
				gid = options.gid;
				flags |= UV_PROCESS_SETGID;
			}
			if(options.detached)
				flags |= UV_PROCESS_DETACHED;
			if(options.windowsVerbatimArguments)
				flags |= UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS;
			if(options.windowsHide)
				flags |= UV_PROCESS_WINDOWS_HIDE;
			if(options.windowsHideConsole)
				flags |= UV_PROCESS_WINDOWS_HIDE_CONSOLE;
			if(options.windowsHideGui)
				flags |= UV_PROCESS_WINDOWS_HIDE_GUI;
		}
		var cOptions = UV.alloc_process_options(cmd.toUTF8(), cArgs, env, cwd, flags, stdioCount, stdio, uid, gid);

		var process = new Process(UV.alloc_process());
		if(options != null && options.onExit != null)
			process.onExit = options.onExit;

		var result = loop.spawn(process.h, cOptions);

		if(cArgs != null) env.free_char_array();
		if(env != null) env.free_char_array();
		if(stdio != null) stdio.free_stdio_container();
		cOptions.free_process_options();

		if(result < 0) {
			process.freeHandle();
			result.throwErr();
		}
		return process;
	}

	/**
		Sends the specified signal to the process.
	**/
	public function kill(sigNum:SigNum):Void {
		handle(h -> h.process_kill(sigNum.translate_to_sys_signal()));
	}

	/**
		Sends the signal to the process with the specified pid.
	**/
	static public function killPid(pid:Int, sigNum:SigNum):Void {
		UV.kill(pid, sigNum.translate_to_sys_signal());
	}

}