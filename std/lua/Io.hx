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
package lua;
import haxe.extern.Rest;

@:native("_G.io")
extern class Io {
	public static var stdin  : FileHandle;
	public static var stderr : FileHandle;
	public static var stdout : FileHandle;

	public static function close(?file : FileHandle) : Void;
	public static function flush() : Void;

	@:overload(   function      (file : String)     : Void {})
	public static function input(file : FileHandle) : Void;

	public static function lines(?file : String) : NativeIterator<String>;

	public static function open (filename : String, ?mode : String) : FileHandle;
	public static function popen(command  : String, ?mode : String) : FileHandle;

	@:overload(   function     (?count    : Int)    : String {})
	public static function read(?filename : String) : String;

	public static function write(v : Rest<String>) : Void;

	public static function output(?file : String) : FileHandle;

	public static function tmpfile() : FileHandle;
	public static function type(obj : FileHandle) : IoType;
}

@:enum
abstract IoType(String) {
	var File = "file";
	var ClosedFile = "closed file";
	var NotAFile = null;
	@:to public function toString(){
		return this;
	}
}
