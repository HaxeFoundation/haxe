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


import python.Exceptions.OSError;
import python.Tuple;
import python.Dict;

extern class Stat {
	public var st_mode:Int;
	public var st_ino:Int;
	public var st_dev:Int;
	public var st_nlink:Int;
	public var st_uid:Int;
	public var st_gid:Int;
	public var st_size:Int;
	public var st_atime:Int;
	public var st_mtime:Int;
	public var st_ctime:Int;

	@:optional public var st_blocks:Int;
	@:optional public var st_blksize:Int;
	@:optional public var st_rdev:Int;
	@:optional public var st_flags:Int;

	@:optional public var st_gen:Int;
	@:optional public var st_birthtime:Int;

	@:optional public var st_rsize:Int;
	@:optional public var st_creator:Int;
	@:optional public var st_type:Int;
}

@:pythonImport("os")
extern class Os {

	public static var environ : Dict<String, String>;

	public static function putenv (name:String, value:String):Void;

	public static function chdir (path:String):Void;

	public static function unlink (path:String):Void;
	public static function remove (path:String):Void;

	public static function getcwd():String;

	public static function getcwdb():Bytes;

	public static function removedirs (path:String):Void;

	public static function rename (src:String, dest:String):Void;

	public static function renames (oldName:String, newName:String):Void;

	public static function rmdir (path:String):Void;


	public static function stat (path:String):Stat;

	public static function fchdir (fd:FileDescriptor):Void;

	public static function listdir (path:String = "."):Array<String>;

	public static function walk (top:String, topdown:Bool = true, onerror:OSError->Void = null, followlinks:Bool = false):Tuple3<String, Array<String>, Array<String>>;

	public static var sep(default, null) : String;
	public static var pathsep(default, null):String;

	public static function makedirs (path:String, mode : Int = 511 /* Oktal 777 */, exist_ok:Bool = false):Void;

	public static function mkdir (path:String, mode : Int = 511 /* Oktal 777 */):Void;
}