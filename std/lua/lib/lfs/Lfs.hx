/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package lua.lib.lfs;
@:luaRequire("lfs")
extern class Lfs {
	@:overload(   function			 (filepath : String, aname : String) : Dynamic {})
	public static function attributes(filepath : String) : LfsFileStat;

	public static function chdir(path : String) : LfsStatus<Bool>;
	public static function lock_dir(path : String, ?second_stale : Int) : LfsStatus<String>; 
	public static function currentdir() : LfsStatus<String>;
	public static function dir(path : String) : Void->String;
	public static function lock(filename : String, mode : String, ?start : Int, ?length : Int) : LfsStatus<Bool>;
	public static function link(old : String, _new :String, ?symlink : Bool) : Void;
	public static function mkdir(dirname : String) : LfsStatus<Bool>;
	public static function rmdir(dirname : String) : LfsStatus<Bool>;
	public static function setmode(file: String, mode : String) : LfsStatus<Bool>;
	public static function symlinkattributes(filepath : String, ?aname : String) : Table<String, String>;
	public static function touch(filepath : String, ?atime : Int, ?mtime : Int) : LfsStatus<Bool>;
	public static function unlock(filehandle : String, ?start : Int, ?length : Int) : LfsStatus<Bool>;
}

@:multiReturn extern class LfsStatus<T> {
	var status : T;
	var message : String;
}

@:multiReturn extern class LfsDir {
	var iter : DirObj->String;
	var dirobj : DirObj;
}

typedef DirObj = {
	next : Void->String,
	close : Void->Void
};
