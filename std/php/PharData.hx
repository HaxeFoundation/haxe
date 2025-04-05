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
	The `PharData` class provides a high-level interface to accessing and creating non-executable TAR and ZIP archives.
	@see https://www.php.net/manual/en/class.phardata.php
**/
@:native("PharData")
extern class PharData extends RecursiveDirectoryIterator implements Countable implements php.ArrayAccess<String, PharFileInfo> {
	function new(fname: String, ?flags: Int, ?alias: String, ?format: Int);

	function addEmptyDir(dirname: String): Void;
	function addFile(file: String, ?localname: String): Void;
	function addFromString(localname: String, contents: String): Void;
	function buildFromDirectory(base_dir: String, ?regex: String): NativeAssocArray<String>;
	function buildFromIterator(iter: EitherType<NativeIterator<String, String>, NativeIterator<Int, SplFileInfo>>, ?base_directory: String): NativeAssocArray<String>;
	function compress(compression: Int, ?extension: String): PharData;
	function compressFiles(compression: Int): Void;
	function copy(oldfile: String, newfile: String): Bool;
	function count(): Int;
	function decompress(?extension: String): PharData;
	function decompressFiles(): Void;
	function delete(entry: String): Bool;
	function delMetadata(): Bool;
	function extractTo(pathto: String, ?files: EitherType<String, NativeIndexedArray<String>>, overwrite: Bool = false): Bool;
	function isWritable(): Bool;
	function offsetExists(offset: String): Bool;
	function offsetGet(offset: String): PharFileInfo;
	function offsetSet(offset: String, value: Dynamic): Void;
	function offsetUnset(offset: String): Void;
	function setAlias(alias: String): Bool;
	function setDefaultStub(?index: String, ?webindex: String): Bool;
	function setMetadata(metadata: Any): Void;
	function setSignatureAlgorithm(sigtype: Int): Void;
	function setStub(stub: String, len: Int = -1): Bool;
}
