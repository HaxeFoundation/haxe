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

import python.Exceptions.BaseException;
import python.lib.io.FileIO;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import python.Tuple;


extern class TB {}
extern class Frame {}

@:pythonImport("sys")
extern class Sys {

	public static var argv(default, never):Array<String>;

	public static var executable(default, never):String;

	public static function exit (x:Int):Void;

	public static function getfilesystemencoding():String;

	public static var version:String;
	public static var platform:String;

	public static var stdout(default, never):TextIOBase;
	public static var stdin(default, never):TextIOBase;
	public static var stderr(default, never):TextIOBase;

	public static function getsizeof (t:Dynamic):Int;

	public static var maxsize:Int;

	public static function exc_info<T:BaseException>():Tuple3<Class<T>, T, TB>;

	public static var version_info:Tuple5<Int,Int,Int,String,Int>;
}
