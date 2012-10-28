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
import haxe.Int64;

@:final @:native('System.IO.FileInfo') extern class FileInfo 
{
	var Attributes(default, null):FileAttributes;
	var CreationTime(default, null):cs.system.DateTime;
	var Directory(default, null):DirectoryInfo;
	var DirectoryName(default, null):String;
	var Exists(default, null):Bool;
	var Extension(default, null):String;
	var FullName(default, null):String;
	var LastAccessTime(default, null):cs.system.DateTime;
	var LastWriteTime(default, null):cs.system.DateTime;
	var Length(default, null):Int64;
	var Name(default, null):String;
	
	function new(filename:String):Void;
}