/*
 * Copyright (C)2005-2016 Haxe Foundation
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

extern class Process {

	var stdout(default,null) : haxe.io.Input;
	var stderr(default,null) : haxe.io.Input;
	var stdin(default,null) : haxe.io.Output;

	/**
		Construct a `Process` object, which run the given command immediately.

		When `args` is not `null`, it will be used as the command arguments,
		with shell meta-characters automatically escaped and quoted if needed.
		`cmd` should be an executable name that can be located in the `PATH` environment variable, or a path to an executable.
		
		When `args` is not given or is `null`, no automatic escaping/quoting is performed.
		`cmd` should include the command together with its arguments, formatted exactly as it would be when typed at the command line.
		It can run executables, as well as shell commands that are not executables (e.g. on Windows: `dir`, `cd`, `echo` etc).

		`close()` should be called when the `Process` is no longer used.
	*/
	function new( cmd : String, ?args : Array<String> ) : Void;

	/**
		Return the process ID.
	*/
	function getPid() : Int;

	/**
		Block until the process exits and return the exit code of the process.
		If the process has already exited, return the exit code immediately.
	*/
	function exitCode() : Int;

	/**
		Close the process handle and release the associated resources.
		All `Process` fields should not be used after `close()` is called.
	*/
	function close() : Void;

	/**
		Kill the process.
	*/
	function kill() : Void;

}