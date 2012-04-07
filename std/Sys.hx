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
extern class Sys {

	/**
		Print any value on the standard output.
	**/
	static function print( v : Dynamic ) : Void;

	/**
		Print any value on the standard output, followed by a newline
	**/
	static function println( v : Dynamic ) : Void;

	/**
		Returns all the arguments that were passed by the commandline.
	**/
	static function args() : Array<String>;

	/**
		Returns the value of the given environment variable.
	**/
	static function getEnv( s : String ) : String;

	/**
		Set the value of the given environment variable.
	**/
	static function putEnv( s : String, v : String ) : Void;

	/**
		Returns the whole environement variables.
	**/
	static function environment() : Hash<String>;

	/**
		Suspend the current execution for the given time (in seconds).
	**/
	static function sleep( seconds : Float ) : Void;

	/**
		Change the current time locale, which will affect [DateTools.format] date formating.
		Returns true if the locale was successfully changed
	**/
	static function setTimeLocale( loc : String ) : Bool;

	/**
		Get the current working directory (usually the one in which the program was started)
	**/
	static function getCwd() : String;

	/**
		Change the current working directory.
	**/
	static function setCwd( s : String ) : Void;

	/**
		Returns the name of the system you are running on. For instance :
			"Windows", "Linux", "BSD" and "Mac" depending on your desktop OS.
	**/
	static function systemName() : String;

	/**
		Run the given command with the list of arguments. The command output will be printed on the same output as the current process.
		The current process will block until the command terminates and it will return the command result (0 if there was no error).
		Read the [sys.io.Process] api for a more complete way to start background processes.
	**/
	static function command( cmd : String, ?args : Array<String> ) : Int;

	/**
		Exit the current process with the given error code.
	**/
	static function exit( code : Int ) : Void;

	/**
		Gives the most precise timestamp value (in seconds).
	**/
	static function time() : Float;

	/**
		Gives the most precise timestamp value (in seconds) but only account for the actual time spent running on the CPU for the current thread/process.
	**/
	static function cpuTime() : Float;

	/**
		Returns the path to the current executable that we are running.
	**/
	static function executablePath() : String;

	/**
		Read a single input character from the standard input (without blocking) and returns it. Setting [echo] to true will also display it on the output.
	**/
	static function getChar( echo : Bool ) : Int;

	/**
		Returns the process standard input, from which you can read what user enters. Usually it will block until the user send a full input line. See [getChar] for an alternative.
	**/
	static function stdin() : haxe.io.Input;

	/**
		Returns the process standard output on which you can write.
	**/
	static function stdout() : haxe.io.Output;

	/**
		Returns the process standard error on which you can write.
	**/
	static function stderr() : haxe.io.Output;

}