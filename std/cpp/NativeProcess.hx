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
extern class NativeProcess {
	@:native("_hx_std_process_run")
	static function process_run(cmd:String, vargs:Array<String>):Dynamic;

	@:native("_hx_std_process_run")
	static function process_run_with_show(cmd:String, vargs:Array<String>, inShow:Int):Dynamic;

	@:native("_hx_std_process_stdout_read")
	static function process_stdout_read(handle:Dynamic, buf:haxe.io.BytesData, pos:Int, len:Int):Int;

	@:native("_hx_std_process_stderr_read")
	static function process_stderr_read(handle:Dynamic, buf:haxe.io.BytesData, pos:Int, len:Int):Int;

	@:native("_hx_std_process_stdin_write")
	static function process_stdin_write(handle:Dynamic, buf:haxe.io.BytesData, pos:Int, len:Int):Int;

	@:native("_hx_std_process_stdin_close")
	static function process_stdin_close(handle:Dynamic):Void;

	@:native("_hx_std_process_exit")
	static function process_exit(handle:Dynamic, block:Bool):Dynamic;

	@:native("_hx_std_process_pid")
	static function process_pid(handle:Dynamic):Int;

	@:native("_hx_std_process_kill")
	static function process_kill(handle:Dynamic):Void;

	@:native("_hx_std_process_close")
	static function process_close(handle:Dynamic):Void;
}
