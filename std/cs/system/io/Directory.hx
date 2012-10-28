/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package cs.system.io;
import cs.NativeArray;
import cs.NativeArray;

@:native('System.IO.Directory') extern class Directory 
{
	static function CreateDirectory(path:String):Void;
	static function Delete(path:String):Void;
	static function Exists(path:String):Bool;
	static function GetCurrentDirectory():String;
	static function GetDirectories(path:String):NativeArray<String>;
	static function GetDirectoryRoot(path:String):String;
	static function GetFiles(path:String):NativeArray<String>;
	static function GetFileSystemEntries(path:String):NativeArray<String>;
	static function GetCreationTime(path:String):cs.system.DateTime;
	static function GetLastAccessTime(path:String):cs.system.DateTime;
	static function GetLastWriteTime(path:String):cs.system.DateTime;
	static function Move(path:String, newpath:String):Void;
	static function SetCurrentDirectory(path:String):Void;
}