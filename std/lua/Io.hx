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
package lua;
import haxe.extern.Rest;

/**
		Input and Output Facilities
**/
@:native("_G.io")
extern class Io {
	public static var stdin  : FileHandle;
	public static var stderr : FileHandle;
	public static var stdout : FileHandle;

	/**
		Function to close regular files.
	**/
	public static function close(?file : FileHandle) : Void;

	/**
		Saves any written data to file.
	**/
	public static function flush() : Void;

	/**
		When called with a file name, it opens the named file (in text mode),
		and sets its handle as the default input file. When called with a file handle,
		it simply sets this file handle as the default input file.
		When called without parameters, it returns the current default input file.

		In case of errors this function raises the error, instead of returning an
		error code.
	**/
	@:overload(   function      (file : String)     : Void {})
	public static function input(file : FileHandle) : Void;

	/**
		Opens the given file name in read mode and returns an iterator function that, 
		each time it is called, returns a new line from the file. 
	**/
	public static function lines(?file : String) : NativeIterator<String>;

	/**
		This function opens a file, in the mode specified in the string mode.
		It returns a new file handle, or, in case of errors, `null` plus an error message.
		
		The mode string can be any of the following:
		
		 * `"r"`: read mode (the default)
		 * `"w"`: write mode
		 * `"a"`: append mode
		 * `"r+"`: update mode, all previous data is preserved
		 * `"w+"`: update mode, all previous data is erased
		 * `"a+"`: append update mode, previous data is preserved, writing is only 
		    allowed at the end of file
		
		The mode string can also have a `b` at the end, which is needed in some systems 
		to open the file in binary mode. This string is exactly what is used in the 
		standard C function fopen.
	**/
	public static function open (filename : String, ?mode : String) : FileHandle;

	/**
		Starts program `command` in a separated process and returns a file handle that 
		you can use to read data from this program (if mode is `"r"`, the default) 
		or to write data to this program (if mode is `"w"`).
		
		This function is system dependent and is not available on all platforms.
	**/
	public static function popen(command  : String, ?mode : String) : FileHandle;

	@:overload(   function     (?count    : Int)    : String {})
	public static function read(?filename : String) : String;

	/**
		Writes the value of each of its arguments to the file. The arguments must 
		be strings or numbers. 
		To write other values, use `Lua.tostring` or `NativeStringTools.format`
		before write.
	**/
	public static function write(v : Rest<String>) : Void;

	public static function output(?file : String) : FileHandle;

	/**
		Returns a handle for a temporary file. This file is opened in update mode
		and it is automatically removed when the program ends.
	**/
	public static function tmpfile() : FileHandle;

	/**
		Checks whether `obj` is a valid file handle. 
	**/
	public static function type(obj : FileHandle) : IoType;
}

/**
	A enumerator that describes the output of `Io.type()`.
**/
@:enum
abstract IoType(String) {
	var File = "file";
	var ClosedFile = "closed file";
	var NotAFile = null;
	@:to public function toString(){
		return this;
	}
}
