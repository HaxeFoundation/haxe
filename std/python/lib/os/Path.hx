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

package python.lib.os;

import haxe.extern.Rest;
import python.Tuple;

@:pythonImport("os", "path")
extern class Path {
	static var sep:String;
	static function exists(path:String):Bool;

	static function abspath(path:String):String;

	static function basename(path:String):String;

	static function commonprefix(paths:Array<String>):String;

	static function lexists(path:String):Bool;

	static function expanduser(path:String):String;

	static function expandvars(path:String):String;

	static function getmtime(path:String):Float;

	static function getatime(path:String):Float;

	static function getctime(path:String):Float;

	static function getsize(path:String):Int;

	static function isabs(path:String):Bool;

	static function isfile(path:String):Bool;

	static function isdir(path:String):Bool;

	static function dirname(path:String):String;

	static function islink(path:String):Bool;

	static function ismount(path:String):Bool;

	static function join(path:String, paths:Rest<String>):String;

	static function normpath(path:String):String;

	static function realpath(path:String):String;

	static function relpath(path:String):String;

	static function samefile(path1:String, path2:String):String;

	static function split(path:String):Tuple2<String, String>;

	static function splitdrive(path:String):Tuple2<String, String>;

	static function splitext(path:String):Tuple2<String, String>;

	static function supports_unicode_filenames():Bool;
}
