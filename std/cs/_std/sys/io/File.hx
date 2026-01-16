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

package sys.io;

@:coreApi
class File {
	public static function getContent(path:String):String {
		return untyped __cs__("System.IO.File.ReadAllText({0})", path);
	}

	public static function saveContent(path:String, content:String):Void {
		untyped __cs__("System.IO.File.WriteAllText({0}, {1})", path, content);
	}

	public static function getBytes(path:String):haxe.io.Bytes {
		var data:Dynamic = untyped __cs__("System.IO.File.ReadAllBytes({0})", path);
		var length:Int = untyped __cs__("{0}.Length", data);
		return haxe.io.Bytes.ofData(data);
	}

	public static function saveBytes(path:String, bytes:haxe.io.Bytes):Void {
		untyped __cs__("System.IO.File.WriteAllBytes({0}, {1})", path, bytes.getData());
	}

	public static function read(path:String, binary:Bool = true):FileInput {
		// FileMode.Open, FileAccess.Read, FileShare.Read
		var stream:Dynamic = untyped __cs__("new System.IO.FileStream({0}, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read)", path);
		return @:privateAccess new FileInput(stream);
	}

	public static function write(path:String, binary:Bool = true):FileOutput {
		// FileMode.Create, FileAccess.Write, FileShare.None
		var stream:Dynamic = untyped __cs__("new System.IO.FileStream({0}, System.IO.FileMode.Create, System.IO.FileAccess.Write, System.IO.FileShare.None)", path);
		return @:privateAccess new FileOutput(stream);
	}

	public static function append(path:String, binary:Bool = true):FileOutput {
		// FileMode.Append, FileAccess.Write, FileShare.None
		var stream:Dynamic = untyped __cs__("new System.IO.FileStream({0}, System.IO.FileMode.Append, System.IO.FileAccess.Write, System.IO.FileShare.None)", path);
		return @:privateAccess new FileOutput(stream);
	}

	public static function update(path:String, binary:Bool = true):FileOutput {
		// FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None
		var stream:Dynamic = untyped __cs__("new System.IO.FileStream({0}, System.IO.FileMode.OpenOrCreate, System.IO.FileAccess.ReadWrite, System.IO.FileShare.None)", path);
		return @:privateAccess new FileOutput(stream);
	}

	public static function copy(srcPath:String, dstPath:String):Void {
		untyped __cs__("System.IO.File.Copy({0}, {1}, true)", srcPath, dstPath);
	}
}
