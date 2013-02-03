/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import java.lang.System;
import sys.io.Process;
/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	This class gives you access to many base functionalities of system platforms. Looks in [sys] sub packages for more system APIs.
**/
@:coreApi class Sys {
	private static var _args:java.NativeArray<String>;
	private static var _env:haxe.ds.StringMap<String>;
	private static var _sysName:String;

	/**
		Print any value on the standard output.
	**/
	public static inline function print( v : Dynamic ) : Void
	{
		java.lang.System.out.print(v);
	}

	/**
		Print any value on the standard output, followed by a newline
	**/
	public static inline function println( v : Dynamic ) : Void
	{
		java.lang.System.out.println(v);
	}

	/**
		Returns all the arguments that were passed by the commandline.
	**/
	public static function args() : Array<String>
	{
		if (_args == null)
			return [];
		return java.Lib.array(_args);
	}

	/**
		Returns the value of the given environment variable.
	**/
	public static function getEnv( s : String ) : String
	{
		return java.lang.System.getenv(s);
	}

	/**
		Set the value of the given environment variable.
		Warning: It is not implemented in Java
	**/
	public static function putEnv( s : String, v : String ) : Void
	{
		//java offers no support for it (!)
		throw "Not implemented in this platform";
	}

	/**
		Returns the whole environement variables.
	**/
	public static function environment() : haxe.ds.StringMap<String>
	{
		if (_env != null)
			return _env;
		var _env = _env = new haxe.ds.StringMap();
		for (mv in java.lang.System.getenv().entrySet())
		{
			_env.set(mv.getKey(), mv.getValue());
		}

		return _env;
	}

	/**
		Suspend the current execution for the given time (in seconds).
	**/
	public static function sleep( seconds : Float ) : Void
	{
		try
			java.lang.Thread.sleep(cast seconds * 1000)
		catch (e:Dynamic)
			throw e;
	}

	/**
		Change the current time locale, which will affect [DateTools.format] date formating.
		Returns true if the locale was successfully changed
	**/
	public static function setTimeLocale( loc : String ) : Bool
	{
		return false;
	}

	/**
		Get the current working directory (usually the one in which the program was started)
	**/
	public static function getCwd() : String
	{
		return new java.io.File(".").getAbsolutePath().substr(0,-1);
	}

	/**
		Change the current working directory.
	**/
	public static function setCwd( s : String ) : Void
	{
		//java offers no support for it (!)
		throw "not implemented";
	}

	/**
		Returns the name of the system you are running on. For instance :
			"Windows", "Linux", "BSD" and "Mac" depending on your desktop OS.
	**/
	public static function systemName() : String
	{
		if (_sysName != null) return _sysName;
		var sname = System.getProperty("os.name").toLowerCase();
		if (sname.indexOf("win") >= 0)
			return _sysName = "Windows";
		if (sname.indexOf("mac") >= 0)
			return _sysName = "Mac";
		if (sname.indexOf("nux") >= 0)
			return _sysName = "Linux";
		if (sname.indexOf("nix") >= 0)
			return _sysName = "BSD";

		return _sysName = System.getProperty("os.name");
	}

	/**
		Run the given command with the list of arguments. The command output will be printed on the same output as the current process.
		The current process will block until the command terminates and it will return the command result (0 if there was no error).
		Read the [sys.io.Process] api for a more complete way to start background processes.
	**/
	public static function command( cmd : String, ?args : Array<String> ) : Int
	{
		var proc:Process = new Process(cmd, args == null ? [] : args);
		var ret = proc.exitCode();
		proc.close();

		return ret;
	}

	/**
		Exit the current process with the given error code.
	**/
	public static function exit( code : Int ) : Void
	{
		System.exit(code);
	}

	/**
		Gives the most precise timestamp value (in seconds).
	**/
	public static function time() : Float
	{
		return cast(System.currentTimeMillis(), Float) / 1000;
	}

	/**
		Gives the most precise timestamp value (in seconds) but only account for the actual time spent running on the CPU for the current thread/process.
	**/
	public static function cpuTime() : Float
	{
		return cast(System.nanoTime(), Float) / 1000000000;
	}

	/**
		Returns the path to the current executable that we are running.
	**/
	public static function executablePath() : String
	{
		return getCwd();
	}

	/**
		Read a single input character from the standard input (without blocking) and returns it. Setting [echo] to true will also display it on the output.
	**/
	public static function getChar( echo : Bool ) : Int
	{
		//TODO
		return throw "Not implemented";
	}

	/**
		Returns the process standard input, from which you can read what user enters. Usually it will block until the user send a full input line. See [getChar] for an alternative.
	**/
	public static function stdin() : haxe.io.Input
	{
		var _in:java.io.InputStream = Reflect.field(System, "in");
		return new java.io.NativeInput(_in);
	}

	/**
		Returns the process standard output on which you can write.
	**/
	public static function stdout() : haxe.io.Output
	{
		return new java.io.NativeOutput(System.out);
	}

	/**
		Returns the process standard error on which you can write.
	**/
	public static function stderr() : haxe.io.Output
	{
		return new java.io.NativeOutput(System.err);
	}

}