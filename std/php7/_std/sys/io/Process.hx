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
package sys.io;

import php.*;
import haxe.io.*;

using StringTools;
using php.Global;

@:forward(iterator)
private abstract ProcessPipes(NativeIndexedArray<Resource>) from NativeIndexedArray<Resource> to NativeIndexedArray<Resource> {
	public var stdin (get,never) : Resource;
	public var stdout (get,never) : Resource;
	public var stderr (get,never) : Resource;

	inline function get_stdin() return this[0];
	inline function get_stdout() return this[1];
	inline function get_stderr() return this[2];
}

private class ReadablePipe extends Input {
	var pipe : Resource;
	var tmpBytes : Bytes;

	public function new(pipe:Resource) {
		this.pipe = pipe;
		tmpBytes = Bytes.alloc(1);
	}

	override public function close() : Void {
		pipe.fclose();
	}

	override public function readByte() : Int {
		if (readBytes(tmpBytes, 0, 1) == 0) throw Error.Blocked;
		return tmpBytes.get(0);
	}

	override public function readBytes( s:Bytes, pos:Int, len:Int ) : Int {
		if(pipe.feof()) throw new Eof();

		var result = pipe.fread(len);
		if(result == "") throw new Eof();
		if(result == false) return throw Error.Custom('Failed to read process output');
		var result:String = result;

		var bytes = Bytes.ofString(result);
		s.blit(pos, bytes, 0, result.length);
		return result.length;
	}
}

private class WritablePipe extends Output {
	var pipe : Resource;
	var tmpBytes : Bytes;

	public function new(pipe:Resource) {
		this.pipe = pipe;
		tmpBytes = Bytes.alloc(1);
	}

	override public function close() : Void {
		pipe.fclose();
	}

	override public function writeByte(c:Int) : Void {
		tmpBytes.set(0, c);
		writeBytes(tmpBytes, 0, 1);
	}

	override public function writeBytes( b : Bytes, pos : Int, l : Int ) : Int {
		var s = b.getString(pos, l);
		if(pipe.feof()) throw new Eof();

		var result = Global.fwrite(pipe, s, l);
		if(result == false) throw Error.Custom('Failed to write to process input');
		return result;
	}
}

class Process {
	/**
		Standard output. The output stream where a process writes its output data.
	**/
	public var stdout(default, null) : Input;

	/**
		Standard error. The output stream to output error messages or diagnostics.
	**/
	public var stderr(default, null) : Input;

	/**
		Standard input. The stream data going into a process.
	**/
	public var stdin(default, null) : Output;

	var process : Resource;
	var pipes : ProcessPipes;
	var pid : Int = -1;
	var running : Bool = true;
	var _exitCode : Int = -1;

	/**
		Construct a `Process` object, which run the given command immediately.

		Command arguments can be passed in two ways: 1. using `args`, 2. appending to `cmd` and leaving `args` as `null`.

		 1. When using `args` to pass command arguments, each argument will be automatically quoted, and shell meta-characters will be escaped if needed.
		`cmd` should be an executable name that can be located in the `PATH` environment variable, or a path to an executable.

		 2. When `args` is not given or is `null`, command arguments can be appended to `cmd`. No automatic quoting/escaping will be performed. `cmd` should be formatted exactly as it would be when typed at the command line.
		It can run executables, as well as shell commands that are not executables (e.g. on Windows: `dir`, `cd`, `echo` etc).

		`close()` should be called when the `Process` is no longer used.
	*/
	public function new( cmd : String, ?args : Array<String> ) : Void {
		var descriptors = Syntax.arrayDecl(
			Syntax.arrayDecl('pipe', 'r'),
			Syntax.arrayDecl('pipe', 'w'),
			Syntax.arrayDecl('pipe', 'w')
		);
		var result = buildCmd(cmd, args).proc_open(descriptors, pipes);
		if (result == false) throw Error.Custom('Failed to start process: $cmd');
		process = result;

		updateStatus();

		stdin = new WritablePipe(pipes.stdin);
		stdout = new ReadablePipe(pipes.stdout);
		stderr = new ReadablePipe(pipes.stderr);
	}

	/**
		Return the process ID.
	*/
	public function getPid() : Int {
		return pid;
	}

	public function exitCode( block : Bool = true ) : Null<Int> {
		if(!block) {
			updateStatus();
			return (running ? null : _exitCode);
		}
		while (running) {
			var arr = Syntax.arrayDecl(process);
			Syntax.suppress(Global.stream_select(arr, arr, arr, null));
			updateStatus();
		}
		return _exitCode;
	}

	/**
		Close the process handle and release the associated resources.
		All `Process` fields should not be used after `close()` is called.
	*/
	public function close() : Void {
		if (!running) return;

		for (pipe in pipes) Global.fclose(pipe);
		process.proc_close();
	}

	/**
		Kill the process.
	*/
	public function kill() : Void {
		process.proc_terminate();
	}

	function buildCmd( cmd:String, ?args:Array<String> ) : String {
		if (args == null) return cmd;

		return switch (Sys.systemName()) {
			case "Windows":
				[cmd.replace("/", "\\")].concat(args).map(StringTools.quoteWinArg.bind(_, true)).join(" ");
			case _:
				[cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
		}
	}

	function updateStatus() : Void {
		if (!running) return;

		var status = process.proc_get_status();
		if (status == false) throw Error.Custom('Failed to obtain process status');
		var status:NativeAssocArray<Scalar> = status;

		pid = status['pid'];
		running = status['running'];
		_exitCode = status['exitcode'];
	}
}