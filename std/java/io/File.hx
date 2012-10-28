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
package java.io;
import haxe.Int64;

extern class File 
{
	@:overload(function(prefix:String, suffix:String, dir:File):File { })
	static function createTempFile(prefix:String, suffix:String):File;
	static function listRoots():java.NativeArray<File>;
	
	function new(pathName:String):Void;
	
	function canRead():Bool;
	function canWrite():Bool;
	function createNewFile():Bool;
	function delete():Bool;
	function deleteOnExit():Void;
	function exists():Bool;
	
	function getAbsoluteFile():File;
	function getAbsolutePath():String;
	function getCanonicalFile():File;
	function getCanonicalPath():String;
	
	function getName():String;
	function getPath():String;
	
	function isAbsolute():Bool;
	function isDirectory():Bool;
	function isFile():Bool;
	function isHidden():Bool;
	function lastModified():Int64;
	function length():Int64;
	
	function list():java.NativeArray<String>;
	function listFiles():java.NativeArray<String>;
	
	function mkdir():Bool;
	function mkdirs():Bool;
	function renameTo(dest:File):Bool;
	function setLastModified(time:Int64):Bool;
	function setReadOnly():Bool;
}