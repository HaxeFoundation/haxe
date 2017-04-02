/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	API for reading and writing to files.

	See `sys.FileSystem` for the complementary file system API.
**/
extern class File {

	/**
		Retrieves the content of the file specified by `path` as a String.

		If the file does not exist or can not be read, an exception is thrown.

		`sys.FileSystem.exists` can be used to check for existence.

		If `path` is null, the result is unspecified.
	**/
	static function getContent( path : String ) : String;

	/**
		Stores `content` in the file specified by `path`.

		If the file cannot be written to, an exception is thrown.

		If `path` or `content` are null, the result is unspecified.
	**/
	static function saveContent( path : String, content : String ) : Void;

	/**
		Retrieves the binary content of the file specified by `path`.

		If the file does not exist or can not be read, an exception is thrown.

		`sys.FileSystem.exists` can be used to check for existence.

		If `path` is null, the result is unspecified.
	**/
	static function getBytes( path : String ) : haxe.io.Bytes;

	/**
		Stores `bytes` in the file specified by `path` in binary mode.

		If the file cannot be written to, an exception is thrown.

		If `path` or `bytes` are null, the result is unspecified.
	**/
	static function saveBytes( path : String, bytes : haxe.io.Bytes ) : Void;

	/**
		Returns an `FileInput` handle to the file specified by `path`.

		If `binary` is true, the file is opened in binary mode. Otherwise it is
		opened in non-binary mode.

		If the file does not exist or can not be read, an exception is thrown.

		Operations on the returned `FileInput` handle read on the opened file.

		File handles should be closed via `FileInput.close` once the operation
		is complete.

		If `path` is null, the result is unspecified.
	**/
	static function read( path : String, binary : Bool = true ) : FileInput;

	/**
		Returns an `FileOutput` handle to the file specified by `path`.

		If `binary` is true, the file is opened in binary mode. Otherwise it is
		opened in non-binary mode.

		If the file cannot be written to, an exception is thrown.

		Operations on the returned `FileOutput` handle write to the opened file.
		If the file existed, its previous content is overwritten.

		File handles should be closed via `FileOutput.close` once the operation
		is complete.

		If `path` is null, the result is unspecified.
	**/
	static function write( path : String, binary : Bool = true ) : FileOutput;

	/**
		Similar to `sys.io.File.write`, but appends to the file if it exists
		instead of overwriting its contents.
	**/
	static function append( path : String, binary : Bool = true ) : FileOutput;


	/**
		Copies the contents of the file specified by `srcPath` to the file
		specified by `dstPath`.

		If the `srcPath` does not exist or cannot be read, or if the `dstPath`
		file cannot be written to, an exception is thrown.

		If the file at `dstPath` exists, its contents are overwritten.

		If `srcPath` or `dstPath` are null, the result is unspecified.
	**/
	static function copy( srcPath : String, dstPath : String ) : Void;
}
