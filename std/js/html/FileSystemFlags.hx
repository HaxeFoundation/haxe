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

// This file is generated from mozilla\FileSystem.webidl. Do not edit!

package js.html;

/**
	The `FileSystemFlags` dictionary defines a set of values which are used when specifying option flags when calling certain methods in the File and Directory Entries API. Methods which accept an options parameter of this type may specify zero or more of these flags as fields in an object, like this:

	Documentation [FileSystemFlags](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemFlags) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FileSystemFlags$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FileSystemFlags>
**/
typedef FileSystemFlags = {
	
	/**
		If this property is `true`, and the requested file or directory doesn't exist, the user agent should create it. The default is `false`. The parent directory must already exist.
	**/
	var ?create : Bool;
	
	/**
		If `true`, and the `create` option is also `true`, the file must not exist prior to issuing the call. Instead, it must be possible for it to be created newly at call time. The default is `false`.
	**/
	var ?exclusive : Bool;
}