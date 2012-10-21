import sys.io.Process;
import cs.system.Environment;
import cs.system.threading.Thread;
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
@:coreApi
class Sys {
	private static var _env:Hash<String>;
	private static var _args:Array<String>;

	/**
		Print any value on the standard output.
	**/
	public static function print( v : Dynamic ) : Void
	{
		cs.system.Console.Write(v);
	}

	/**
		Print any value on the standard output, followed by a newline
	**/
	public static function println( v : Dynamic ) : Void
	{
		cs.system.Console.WriteLine(v);
	}

	/**
		Returns all the arguments that were passed by the commandline.
	**/
	public static function args() : Array<String>
	{
		if (_args == null)
		{
			var ret = cs.Lib.array(Environment.GetCommandLineArgs());
			ret.shift();
			_args = ret;
		}
		return _args.copy();
	}

	/**
		Returns the value of the given environment variable.
	**/
	public static function getEnv( s : String ) : String
	{
		return Environment.GetEnvironmentVariable(s);
	}

	/**
		Set the value of the given environment variable.
	**/
	public static function putEnv( s : String, v : String ) : Void
	{
		Environment.SetEnvironmentVariable(s, v);
		if (_env != null)
			_env.set(s, v);
	}

	/**
		Returns the whole environement variables.
	**/
	public static function environment() : Hash<String>
	{
		if (_env == null)
		{
			var e = _env = new Hash();
			var nenv = Environment.GetEnvironmentVariables().GetEnumerator();
			while (nenv.MoveNext())
			{
				e.set(nenv.Key, nenv.Value);
			}
		}

		return _env;
	}

	/**
		Suspend the current execution for the given time (in seconds).
	**/
	public static function sleep( seconds : Float ) : Void
	{
		Thread.Sleep( Std.int(seconds * 1000) );
	}

	/**
		Change the current time locale, which will affect [DateTools.format] date formating.
		Returns true if the locale was successfully changed
	**/
	public static function setTimeLocale( loc : String ) : Bool
	{
		//TODO C#
		return false;
	}

	/**
		Get the current working directory (usually the one in which the program was started)
	**/
	public static function getCwd() : String
	{
		return cs.system.io.Directory.GetCurrentDirectory();
	}

	/**
		Change the current working directory.
	**/
	public static function setCwd( s : String ) : Void
	{
		cs.system.io.Directory.SetCurrentDirectory(s);
	}

	/**
		Returns the name of the system you are running on. For instance :
			"Windows", "Linux", "BSD" and "Mac" depending on your desktop OS.
	**/
	public static function systemName() : String
	{
		//doing a switch with strings since MacOS might not be available
		switch(Environment.OSVersion.Platform + "")
		{
			case "Unix": return "Linux";
			case "Xbox": return "Xbox";
			case "MacOSX": return "Mac";
			default:
				var ver = cast(Environment.OSVersion.Platform, Int);
				if (ver == 4 || ver == 6 || ver == 128)
					return "Linux";
				return "Windows";
		}
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
		Environment.Exit(code);
	}

	/**
		Gives the most precise timestamp value (in seconds).
	**/
	public static function time() : Float
	{
		return Date.now().getTime();
	}

	/**
		Gives the most precise timestamp value (in seconds) but only account for the actual time spent running on the CPU for the current thread/process.
	**/
	public static function cpuTime() : Float
	{
		return Environment.TickCount / 1000;
	}

	/**
		Returns the path to the current executable that we are running.
	**/
	public static function executablePath() : String
	{
		//TODO: add extern references
		return untyped __cs__('System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase');
	}

	/**
		Read a single input character from the standard input (without blocking) and returns it. Setting [echo] to true will also display it on the output.
	**/
	public static function getChar( echo : Bool ) : Int
	{
		#if !(Xbox || CF || MF) //Xbox, Compact Framework, Micro Framework
		return untyped __cs__('((int) System.Console.ReadKey(!echo).KeyChar)');
		#else
		return -1;
		#end
	}

	/**
		Returns the process standard input, from which you can read what user enters. Usually it will block until the user send a full input line. See [getChar] for an alternative.
	**/
	public static function stdin() : haxe.io.Input
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeInput(cs.system.Console.OpenStandardInput());
#else
		return null;
#end
	}

	/**
		Returns the process standard output on which you can write.
	**/
	public static function stdout() : haxe.io.Output
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeOutput(cs.system.Console.OpenStandardOutput());
#else
		return null;
#end
	}

	/**
		Returns the process standard error on which you can write.
	**/
	public static function stderr() : haxe.io.Output
	{
#if !(Xbox || CF || MF)
		return new cs.io.NativeOutput(cs.system.Console.OpenStandardError());
#else
		return null;
#end
	}

}