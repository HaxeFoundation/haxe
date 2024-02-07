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

/**
	This class provides access to various base functions of system platforms.
	Look in the `sys` package for more system APIs.
**/
@:require(sys)
extern class Sys {
	/**
		Prints any value to the standard output.
	**/
	static function print(v:Dynamic):Void;

	/**
		Prints any value to the standard output, followed by a newline.
		On Windows, this function outputs a CRLF newline.
		LF newlines are printed on all other platforms.
	**/
	static function println(v:Dynamic):Void;

	/**
		Returns all the arguments that were passed in the command line.
		This does not include the interpreter or the name of the program file.

		(java)(eval) On Windows, non-ASCII Unicode arguments will not work correctly.
	**/
	static function args():Array<String>;

	/**
		Returns the value of the given environment variable, or `null` if it
		doesn't exist.
	**/
	static function getEnv(s:String):String;

	/**
		Sets the value of the given environment variable.

		If `v` is `null`, the environment variable is removed.

		(java) This functionality is not available on Java; calling this function will throw.
	**/
	static function putEnv(s:String, v:Null<String>):Void;

	/**
		Returns a map of the current environment variables and their values
		as of the invocation of the function.

		(python) On Windows, the variable names are always in upper case.

		(cpp)(hl)(neko) On Windows, the variable names match the last capitalization used when modifying
		the variable if the variable has been modified, otherwise they match their capitalization at
		the start of the process.

		On Windows on remaining targets, variable name capitalization matches however they were capitalized
		at the start of the process or at the moment of their creation.
	**/
	static function environment():Map<String, String>;

	/**
		Suspends execution for the given length of time (in seconds).
	**/
	static function sleep(seconds:Float):Void;

	/**
		Changes the current time locale, which will affect `DateTools.format` date formating.
		Returns `true` if the locale was successfully changed.
	**/
	static function setTimeLocale(loc:String):Bool;

	/**
		Gets the current working directory (usually the one in which the program was started).
	**/
	static function getCwd():String;

	/**
		Changes the current working directory.

		(java) This functionality is not available on Java; calling this function will throw.
	**/
	static function setCwd(s:String):Void;

	/**
		Returns the type of the current system. Possible values are:
		 - `"Windows"`
		 - `"Linux"`
		 - `"BSD"`
		 - `"Mac"`
	**/
	static function systemName():String;

	/**
		Runs the given command. The command output will be printed to the same output as the current process.
		The current process will block until the command terminates.
		The return value is the exit code of the command (usually `0` indicates no error).

		Command arguments can be passed in two ways:

		 1. Using `args` to pass command arguments. Each argument will be automatically quoted and shell meta-characters will be escaped if needed.
		`cmd` should be an executable name that can be located in the `PATH` environment variable, or a full path to an executable.

		 2. When `args` is not given or is `null`, command arguments can be appended to `cmd`. No automatic quoting/escaping will be performed. `cmd` should be formatted exactly as it would be when typed at the command line.
		It can run executables, as well as shell commands that are not executables (e.g. on Windows: `dir`, `cd`, `echo` etc).

		Use the `sys.io.Process` API for more complex tasks, such as background processes, or providing input to the command.
	**/
	static function command(cmd:String, ?args:Array<String>):Int;

	/**
		Exits the current process with the given exit code.

		(macro)(eval) Being invoked in a macro or eval context (e.g. with `-x` or `--run`) immediately terminates
		the compilation process, which also prevents the execution of any `--next` sections of compilation arguments.
	**/
	static function exit(code:Int):Void;

	/**
		Gives the most precise timestamp value available (in seconds).
	**/
	static function time():Float;

	/**
		Gives the most precise timestamp value available (in seconds),
		but only accounts for the actual time spent running on the CPU for the current thread/process.
	**/
	static function cpuTime():Float;

	/**
		Returns the path to the current executable that we are running.
	**/
	@:deprecated("Use programPath instead") static function executablePath():String;

	/**
		Returns the absolute path to the current program file that we are running.
		Concretely, for an executable binary, it returns the path to the binary.
		For a script (e.g. a PHP file), it returns the path to the script.
	**/
	static function programPath():String;

	/**
		Reads a single input character from the standard input and returns it.
		Setting `echo` to `true` will also display the character on the output.
	**/
	static function getChar(echo:Bool):Int;

	/**
		Returns the standard input of the process, from which user input can be read.
		Usually it will block until the user sends a full input line.
		See `getChar` for an alternative.
	**/
	static function stdin():haxe.io.Input;

	/**
		Returns the standard output of the process, to which program output can be written.
	**/
	static function stdout():haxe.io.Output;

	/**
		Returns the standard error of the process, to which program errors can be written.
	**/
	static function stderr():haxe.io.Output;
}
