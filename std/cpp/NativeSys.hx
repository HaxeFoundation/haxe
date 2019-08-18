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

package cpp;

@:buildXml('<include name="${HXCPP}/src/hx/libs/std/Build.xml"/>')
extern class NativeSys {
	@:native("__hxcpp_print")
	public static function print(v:Dynamic):Void;

	@:native("__hxcpp_println")
	public static function println(v:Dynamic):Void;

	@:native("_hx_std_get_env")
	extern public static function get_env(v:String):String;

	@:native("_hx_std_put_env")
	extern public static function put_env(e:String, v:String):Void;

	@:native("_hx_std_sys_sleep")
	extern public static function sys_sleep(f:Float):Void;

	@:native("_hx_std_set_time_locale")
	extern public static function set_time_locale(l:String):Bool;

	@:native("_hx_std_get_cwd")
	extern public static function get_cwd():String;

	@:native("_hx_std_set_cwd")
	extern public static function set_cwd(d:String):Void;

	@:native("_hx_std_sys_string")
	extern public static function sys_string():String;

	@:native("_hx_std_sys_is64")
	extern public static function sys_is64():Bool;

	@:native("_hx_std_sys_command")
	extern public static function sys_command(cmd:String):Int;

	@:native("_hx_std_sys_exit")
	extern public static function sys_exit(code:Int):Void;

	@:native("_hx_std_sys_exists")
	extern public static function sys_exists(path:String):Bool;

	@:native("_hx_std_file_delete")
	extern public static function file_delete(path:String):Void;

	@:native("_hx_std_sys_rename")
	extern public static function sys_rename(path:String, newname:String):Bool;

	@:native("_hx_std_sys_stat")
	extern public static function sys_stat(path:String):Dynamic;

	@:native("_hx_std_sys_file_type")
	extern public static function sys_file_type(path:String):String;

	@:native("_hx_std_sys_create_dir")
	extern public static function sys_create_dir(path:String, mode:Int):Bool;

	@:native("_hx_std_sys_remove_dir")
	extern public static function sys_remove_dir(path:String):Void;

	@:native("_hx_std_sys_time")
	extern public static function sys_time():Float;

	@:native("_hx_std_sys_cpu_time")
	extern public static function sys_cpu_time():Float;

	@:native("_hx_std_sys_read_dir")
	extern public static function sys_read_dir(p:String):Array<String>;

	@:native("_hx_std_file_full_path")
	extern public static function file_full_path(path:String):String;

	@:native("_hx_std_sys_exe_path")
	extern public static function sys_exe_path():String;

	@:native("_hx_std_sys_env")
	extern public static function sys_env():Array<String>;

	@:native("_hx_std_sys_getch")
	extern public static function sys_getch(b:Bool):Int;

	@:native("_hx_std_sys_get_pid")
	extern public static function sys_get_pid():Int;
}
