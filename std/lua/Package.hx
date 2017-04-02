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

/**
  Externs for lua package handling
 **/
@:native("_G.package")
extern class Package {
	/**
		A string describing some compile-time configurations for packages. 
	**/
	public static var config : String;

	/**
		The path used by require to search for a Lua loader.
	**/
	public static var path : String;

	/**
		The path used by require to search for a C loader.
	**/
	public static var cpath : String;

	/**
		A table used by require to control which modules are already loaded. 
	**/
	public static var loaded : Table<String, Bool>;

	/**
		A table to store loaders for specific modules.
	**/
	public static var preload : Table<String, Bool>;

	/**
		A table used by require to control how to load modules.
		Each entry in this table is a searcher function. 
	**/
	public static var searchers :Table<Int,Void->Null<String>>;

	/**
		Searches for the given `libname` in the given path `funcname`.
		A path is a string containing a sequence of templates separated by semicolons. 
	**/
	public static function searchpath(name : String, path : String, ?sep : String, ?rep : String) : Null<String>;

	/**
		Dynamically links the host program with the C library `libname`.
	**/
	public static function loadlib(libname : String, funcname : String) : Void;
}
