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
package python.lib;

import haxe.extern.EitherType;

extern class StartupInfo {
	public var dwFlags : Int;

	public var wShowWindow:Int;

}

@:pythonImport("subprocess")
extern class Subprocess {

	public static function STARTUPINFO():StartupInfo;

	public static var STD_INPUT_HANDLE:Int;
	public static var STD_OUTPUT_HANDLE:Int;
	public static var STD_ERROR_HANDLE:Int;
	public static var SW_HIDE:Int;
	public static var STARTF_USESTDHANDLES:Int;
	public static var STARTF_USESHOWWINDOW:Int;

	public static var CREATE_NEW_CONSOLE:Int;
	public static var CREATE_NEW_PROCESS_GROUP:Int;

	public static var PIPE:Int;

	public static var STDOUT:Int;

	public static function call(args:EitherType<String,Array<String>>, ?kwArgs:python.KwArgs<Dynamic>):Int;

}