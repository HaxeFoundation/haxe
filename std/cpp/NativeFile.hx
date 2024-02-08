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
extern class NativeFile {
	@:native("_hx_std_file_open")
	extern static function file_open(fname:String, r:String):Dynamic;

	@:native("_hx_std_file_close")
	extern static function file_close(handle:Dynamic):Void;

	@:native("_hx_std_file_write")
	extern static function file_write(handle:Dynamic, s:haxe.io.BytesData, p:Int, n:Int):Int;

	@:native("_hx_std_file_write_char")
	extern static function file_write_char(handle:Dynamic, c:Int):Void;

	@:native("_hx_std_file_read")
	extern static function file_read(handle:Dynamic, s:haxe.io.BytesData, p:Int, n:Int):Int;

	@:native("_hx_std_file_read_char")
	extern static function file_read_char(handle:Dynamic):Int;

	@:native("_hx_std_file_seek")
	extern static function file_seek(handle:Dynamic, pos:Int, kind:Int):Void;

	@:native("_hx_std_file_tell")
	extern static function file_tell(handle:Dynamic):Int;

	@:native("_hx_std_file_eof")
	extern static function file_eof(handle:Dynamic):Bool;

	@:native("_hx_std_file_flush")
	extern static function file_flush(handle:Dynamic):Void;

	@:native("_hx_std_file_contents_string")
	extern static function file_contents_string(name:String):String;

	@:native("_hx_std_file_contents_bytes")
	extern static function file_contents_bytes(name:String):haxe.io.BytesData;

	@:native("_hx_std_file_stdin")
	extern static function file_stdin():Dynamic;

	@:native("_hx_std_file_stdout")
	extern static function file_stdout():Dynamic;

	@:native("_hx_std_file_stderr")
	extern static function file_stderr():Dynamic;
}
