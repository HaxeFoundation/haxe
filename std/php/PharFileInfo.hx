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

/**
	The `PharFileInfo` class provides a high-level interface to the contents and attributes of a single file within a PHAR archive.
	@see https://www.php.net/manual/en/class.pharfileinfo
**/
@:native("PharFileInfo")
extern class PharFileInfo extends SplFileInfo {
	function new(entry: String);

	function chmod(permissions: Int): Void;
	function compress(compression: Int): Bool;
	function decompress(): Bool;
	function delMetadata(): Bool;
	function getCompressedSize(): Int;
	function getContent(): String;
	function getCRC32(): Int;
	function getMetadata(): Dynamic;
	function getPharFlags(): Int;
	function hasMetadata(): Bool;
	function isCompressed(?compression_type: Int): Bool;
	function isCRCChecked(): Bool;
	function setMetadata(metadata: Any): Void;
}
