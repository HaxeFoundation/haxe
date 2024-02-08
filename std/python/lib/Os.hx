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

package python.lib;

import python.Exceptions.OSError;
import python.Tuple;
import python.Dict;

extern class Stat {
	var st_mode:Int;
	var st_ino:Int;
	var st_dev:Int;
	var st_nlink:Int;
	var st_uid:Int;
	var st_gid:Int;
	var st_size:Int;
	var st_atime:Int;
	var st_mtime:Int;
	var st_ctime:Int;

	@:optional var st_blocks:Int;
	@:optional var st_blksize:Int;
	@:optional var st_rdev:Int;
	@:optional var st_flags:Int;

	@:optional var st_gen:Int;
	@:optional var st_birthtime:Int;

	@:optional var st_rsize:Int;
	@:optional var st_creator:Int;
	@:optional var st_type:Int;
}

@:pythonImport("os")
extern class Os {
	static var environ:Dict<String, String>;

	static function putenv(name:String, value:String):Void;

	/** Removes the value for the environment variable `name`.

		When targeting python versions prior to 3.9, this function may not exist on some platforms.
	 **/
	static function unsetenv(name:String):Void;

	static function chdir(path:String):Void;

	static function unlink(path:String):Void;
	static function remove(path:String):Void;

	static function getcwd():String;

	static function getcwdb():Bytes;

	static function removedirs(path:String):Void;

	static function rename(src:String, dest:String):Void;

	static function renames(oldName:String, newName:String):Void;

	static function rmdir(path:String):Void;

	static function stat(path:String):Stat;

	static function fchdir(fd:FileDescriptor):Void;

	static function listdir(path:String = "."):Array<String>;

	static function walk(top:String, topdown:Bool = true, onerror:OSError->Void = null,
		followlinks:Bool = false):Tuple3<String, Array<String>, Array<String>>;

	static var sep(default, null):String;
	static var pathsep(default, null):String;

	static function makedirs(path:String, mode:Int = 511 /* Oktal 777 */, exist_ok:Bool = false):Void;

	static function mkdir(path:String, mode:Int = 511 /* Oktal 777 */):Void;
}
