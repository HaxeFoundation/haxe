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
import haxe.extern.Rest;

/**
	The `SplFileObject` class offers an object-oriented interface for a file.
	@see https://www.php.net/manual/en/class.splfileobject.php
**/
@:native("SplFileObject")
extern class SplFileObject extends SplFileInfo implements RecursiveIterator<Int, Dynamic> implements SeekableIterator<Int, Dynamic> {
	@:phpClassConst static final DROP_NEW_LINE: Int;
	@:phpClassConst static final READ_AHEAD: Int;
	@:phpClassConst static final READ_CSV: Int;
	@:phpClassConst static final SKIP_EMPTY: Int;

	function new(filename: String, mode: String = "r", useIncludePath: Bool = false, ?context: Resource);

	function current(): EitherType<String, EitherType<NativeIndexedArray<String>, Bool>>;
	function eof(): Bool;
	function fflush(): Bool;
	function fgetc(): EitherType<String, Bool>;
	function fgetcsv(separator: String = ",", enclosure: String = "\"", escape: String = "\\"): EitherType<NativeIndexedArray<String>, Bool>;
	function fgets(): String;
	function fgetss(?allowable_tags: String): String;
	function flock(operation: Int, ?wouldBlock: Ref<Int>): Bool;
	function fpassthru(): Int;
	function fputcsv(fields: NativeIndexedArray<String>, separator: String = ",", enclosure: String = "\"", escape: String = "\\"): EitherType<Int, Bool>;
	function fread(length: Int): EitherType<String, Bool>;
	function fscanf(format: String, vars: Rest<Ref<Dynamic>>): EitherType<Int, NativeIndexedArray<Dynamic>>;
	function fseek(offset: Int, ?whence: Int): Int;
	function fstat(): NativeArray;
	function ftell(): EitherType<Int, Bool>;
	function ftruncate(size: Int): Bool;
	function fwrite(data: String, length: Int = 0): EitherType<Int, Bool>;
	function getChildren(): Null<RecursiveIterator<Int, Dynamic>>;
	function getCsvControl(): NativeIndexedArray<String>;
	function getFlags(): Int;
	function getMaxLineLen(): Int;
	function hasChildren(): Bool;
	function key(): Int;
	function next(): Void;
	function rewind(): Void;
	function seek(line: Int): Void;
	function setCsvControl(separator: String = ",", enclosure: String = "\"", escape: String = "\\"): Void;
	function setFlags(flags: Int): Void;
	function setMaxLineLen(maxLength: Int): Void;
	function valid(): Bool;
}
