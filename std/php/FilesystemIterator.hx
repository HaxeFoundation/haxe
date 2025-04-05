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
	The Filesystem iterator.
	@see https://www.php.net/manual/en/class.filesystemiterator.php
**/
@:native("FilesystemIterator")
extern class FilesystemIterator extends DirectoryIterator implements SeekableIterator<String, Dynamic> {
	@:phpClassConst static final CURRENT_AS_FILEINFO: Int;
	@:phpClassConst static final CURRENT_AS_PATHNAME: Int;
	@:phpClassConst static final CURRENT_AS_SELF: Int;
	@:phpClassConst static final CURRENT_MODE_MASK: Int;
	@:phpClassConst static final FOLLOW_SYMLINKS: Int;
	@:phpClassConst static final KEY_AS_FILENAME: Int;
	@:phpClassConst static final KEY_AS_PATHNAME: Int;
	@:phpClassConst static final KEY_MODE_MASK: Int;
	@:phpClassConst static final NEW_CURRENT_AND_KEY: Int;
	@:phpClassConst static final SKIP_DOTS: Int;
	@:phpClassConst static final UNIX_PATHS: Int;

	function new(directory: String, ?flags: Int);

	function current(): EitherType<FilesystemIterator, EitherType<SplFileInfo, String>>;
	function getFlags(): Int;
	function key(): String;
	function setFlags(flags: Int): Void;
}
