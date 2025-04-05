/*
 * Copyright (C)2005-2021 Haxe Foundation
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

package php;

import haxe.extern.EitherType;

/**
	The `SplFileInfo` class offers a high-level object-oriented interface to information for an individual file.
	@see https://www.php.net/manual/en/class.splfileinfo.php
**/
@:native("SplFileInfo")
extern class SplFileInfo {
	function new(filename: String);

	function getATime(): EitherType<Int, Bool>;
	function getBasename(suffix: String = ""): String;
	function getCTime(): EitherType<Int, Bool>;
	function getExtension(): String;
	function getFileInfo(?className: String): SplFileInfo;
	function getFilename(): String;
	function getGroup(): EitherType<Int, Bool>;
	function getInode(): EitherType<Int, Bool>;
	function getLinkTarget(): EitherType<String, Bool>;
	function getMTime(): EitherType<Int, Bool>;
	function getOwner(): EitherType<Int, Bool>;
	function getPath(): String;
	function getPathInfo(?className: String): Null<SplFileInfo>;
	function getPathname(): String;
	function getPerms(): EitherType<Int, Bool>;
	function getRealPath(): EitherType<String, Bool>;
	function getSize(): EitherType<Int, Bool>;
	function getType(): EitherType<SplFileInfoType, Bool>;
	function isDir(): Bool;
	function isExecutable(): Bool;
	function isFile(): Bool;
	function isLink(): Bool;
	function isReadable(): Bool;
	function isWritable(): Bool;
	function openFile(mode: String = "r", useIncludePath: Bool = false, ?context: Resource): SplFileObject;
	function setFileClass(?className: String): Void;
	function setInfoClass(?className: String): Void;
}

enum abstract SplFileInfoType(String) to String {
	var Block = "block";
	var Char = "char";
	var Dir = "dir";
	var Fifo = "fifo";
	var File = "file";
	var Link = "link";
	var Socket = "socket";
	var Unknown = "unknown";
}
