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

package cpp.uv;

import cpp.uv.Pipe;
import cpp.uv.Signal;

using cpp.uv.UV;


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
	FD(file:File);
	/**
		Connect child proces descriptor to the specified pipe.

		Specifying `nonBlock` opens the handle in non-blocking mode in the child.
		This may cause loss of data, if the child is not designed to handle to
		encounter this mode, but can also be significantly more efficient.
	**/
	PIPE(pipe:Pipe, permissions:PipeMode, ?nonBlock:Bool);
	/**
		Connect child proces descriptor to the specified stream.
	**/
	STREAM(stream:Stream);
}

/**
	Options for spawning processes.
**/
typedef ProcessOptions = {
	/** Callback called after the process exits. */
	var ?onExit:(p:Process, exitStatus:Int64, termSignal:SigNum)->Void;
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
	Process handles will spawn a new process and allow the user to control it and
	establish communication channels with it using streams.

	@see http://docs.libuv.org/en/v1.x/process.html
**/
@:headerCode('#include "uv.h"')
class Process extends Handle {
	var onExit:(p:Process, exitStatus:Int64, termSignal:SigNum)->Void;
	var uvProcess(get,never):RawPointer<UvProcessT>;

	inline function get_uvProcess():RawPointer<UvProcessT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvProcessT.create();
		super.setupUvData();
	}

	/** The PID of the spawned process. It’s set after calling `spawn()`. */
	public var pid(get,never):Int;
	function get_pid():Int {
		return UV.process_get_pid(uvProcess);
	}

	/**
		Disables (tries) file descriptor inheritance for inherited descriptors.

		The effect is that child processes spawned by this process don’t accidentally
		inherit these handles.
	**/
	static public function disableStdioInheritance():Void {
		UV.disable_stdio_inheritance();
	}

	/**
		Initializes the process handle and starts the process.

		`cmd` is the program to be executed.
		`args[0]` should be the path to the program.
	**/
	static public function spawn(loop:Loop, cmd:String, args:Array<String>, ?options:ProcessOptions):Process {
		var process = new Process(loop);
		var cOpts = new UvProcessOptionsT();
		var cCmd:ConstCharStar = cmd;

		cOpts.file = cmd;
		cOpts.args = args.toChars();
		if(options != null) {
			if(options.onExit != null) {
				cOpts.exit_cb = Callable.fromStaticFunction(uvExitCb);
				process.onExit = options.onExit;
			}
			if(options.env != null)
				cOpts.env = [for(key => value in options.env) '$key=$value'].toChars();
			if(options.stdio != null) {
				cOpts.stdio_count = options.stdio.length;
				var stdio:Pointer<UvStdioContainerT> = Stdlib.malloc(cOpts.stdio_count * untyped __cpp__('sizeof(uv_stdio_container_t)'));
				cOpts.stdio = stdio.raw;
				for(i => io in options.stdio) {
					var cIo = stdio.at(i);
					switch io {
						case IGNORE:
							cIo.flags = UV_IGNORE;
						case INHERIT:
							cIo.flags = UV_INHERIT_FD;
							cIo.data.fd = i;
						case FD(file):
							cIo.flags = UV_INHERIT_FD;
							cIo.data.fd = file.uvFile;
						case PIPE(pipe, permissions, nonBlock):
							var flags:Int = UV_CREATE_PIPE;
							if(nonBlock)
								flags |= UV_NONBLOCK_PIPE;
							switch permissions {
								case READ: flags |= UV_READABLE_PIPE;
								case WRITE: flags |= UV_WRITABLE_PIPE;
								case READ_WRITE: flags |= UV_READABLE_PIPE | UV_WRITABLE_PIPE;
							}
							cIo.flags = untyped __cpp__('(uv_stdio_flags){0}', flags);
							cIo.data.stream = cast pipe.uvStream;
						case STREAM(stream):
							cIo.flags = UV_INHERIT_STREAM;
							cIo.data.stream = cast stream.uvStream;
					}
				}
			}
			var flags = 0;
			if( options.cwd != null ) {
				cOpts.cwd = (options.cwd:String);
			}
			if(options.uid != null) {
				cOpts.uid = options.uid;
				flags |= UV_PROCESS_SETUID;
			}
			if(options.gid != null) {
				cOpts.gid = options.gid;
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
			cOpts.flags = flags;
		}
		var result = UV.spawn(loop.uvLoop, process.uvProcess, RawPointer.addressOf(cOpts));

		Stdlib.free(Pointer.fromRaw(cOpts.args));
		if(cOpts.env != null)
			Stdlib.free(Pointer.fromRaw(cOpts.env));
		if(cOpts.stdio != null)
			Stdlib.free(Pointer.fromRaw(cOpts.stdio));

		result.resolve();
		return process;
	}

	static function uvExitCb(uvProcess:RawPointer<UvProcessT>, exitStatus:Int64, termSignal:Int) {
		var process:Process = cast Handle.get(cast uvProcess);
		var cb = process.onExit;
		process.onExit = null;
		cb(process, exitStatus, Signal.fromInt(termSignal));
	}

	/**
		Sends the specified signal to the process.
	**/
	public function kill(sigNum:SigNum):Void {
		UV.process_kill(uvProcess, sigNum.toInt());
	}

	/**
		Sends the specified signal to the given PID.
	**/
	static public function killPid(pid:Int, sigNum:SigNum) {
		UV.kill(pid, sigNum.toInt()).resolve();
	}
}