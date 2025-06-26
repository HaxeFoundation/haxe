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

package lua;

/**
	Operating System Facilities.
**/
@:native("_G.os")
extern class Os {
	/**
		Returns an approximation of the amount in seconds of CPU time used by the
		program.
	**/
	static function clock():Float;

	@:overload(function(format:String, time:Time):DateType {})
	@:overload(function(format:String):DateType {})
	static function date():DateType;

	/**
		Returns the number of seconds from time `t1` to time `t2`.
		In POSIX, Windows, and some other systems, this value is exactly `t2-t1`.
	**/
	static function difftime(t2:Time, t1:Time):Float;

	// TODO: multi-return

	/**
		This function is equivalent to the C function system. It passes command to
		be executed by an operating system shell. It returns a status code,
		which is system-dependent. If command is absent, then it returns
		nonzero if a shell is available and zero otherwise.
	**/
	#if (lua_ver < 5.2)
	static function execute(?command:String):Int;
	#elseif (lua_ver >= 5.2)
	static function execute(?command:String):OsExecute;
	#else
	static function execute(?command:String):Dynamic;
	#end

	/**
		Calls the C function exit, with an optional code, to terminate the host program.
		The default value for code is the success code.
	**/
	static function exit(code:Int):Int;

	/**
		Returns the value of the process environment variable `varname`, or `null`
		if the variable is not defined.
	**/
	static function getenv(varname:String):String;

	/**
		Deletes the file or directory with the given name.
		Directories must be empty to be removed.
	**/
	static function remove(filename:String):OsSuccess;

	/**
		Renames file or directory named `oldname` to `newname`.
	**/
	static function rename(oldname:String, newname:String):OsSuccess;

	/**
		Sets the current locale of the program.
	**/
	static function setlocale(locale:String, ?category:LocaleCategory):String;

	/**
		Returns the current time when called without arguments, or a time
		representing the date and time specified by the given table.

		The returned value is a number, whose meaning depends on your system.
		In POSIX, Windows, and some other systems, this number counts the number
		of seconds since some given start time (the "epoch").
		In other systems, the meaning is not specified, and the number returned
		by time can be used only as an argument to date and difftime.
	**/
	static function time(?arg:TimeParam):Time;

	/**
		Returns a string with a file name that can be used for a temporary file.
		The file must be explicitly opened before its use and explicitly removed
		when no longer needed.

		When possible, you may prefer to use `Io.tmpfile`, which automatically
		removes the file when the program ends.
	**/
	static function tmpname():String;
}

/**
	A typedef that matches the date parameter `Os.time()` will accept.
**/
typedef TimeParam = {
	year:Int,
	month:Int,
	day:Int,
	?hour:Int,
	?min:Int,
	?sec:Int,
	?isdst:Bool
}

/**
	A typedef that describes the output of `Os.date()`.
**/
typedef DateType = {
	hour:Int,
	min:Int,
	sec:Int,
	isdst:Bool,
	year:Int,
	month:Int,
	wday:Int,
	yday:Int,
	day:Int,
}

@:multiReturn extern class OsExecute {
	var success:Bool;
	var output:String;
	var status:Int;
}

@:multiReturn extern class OsSuccess {
	var success:Bool;
	var message:String;
}
