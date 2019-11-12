/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
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

package lua.lib.luv;

@:luaRequire("luv")
extern class Os {
    @:native("os_homedir")
	public static function homedir():String;

    @:native("os_tmpdir")
	public static function tmpdir():String;

    @:native("os_get_passwd")
	public static function get_passwd():String;

    @:native("os_getenv")
	public static function getenv(env:String):String;

    @:native("os_setenv")
	public static function setenv(env:String, value:String):Void;

    @:native("os_unsetenv")
	public static function unsetenv(env:String):Void;

    @:native("os_gethostname")
    public static function gethostname():String;

    @:native("os_environ")
    public static function environ() : Table<String,String>;

    @:native("os_uname")
    public static function uname() : Uname;

    @:native("os_getpid")
    public static function getpid() : Int;

    @:native("os_getppid")
    public static function getppid() : Int;

    @:native("os_getpriority")
    public static function getpriority(pid :Int) : Int;

    @:native("os_setpriority")
    public static function setpriority(pid :Int, priority : Int) : Bool;

}

typedef Uname = {
    sysname : String,
    release : String,
    version : String,
    machine : String
}

