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

package cpp.uv;

import haxe.io.Bytes;

using cpp.uv.UV;

/**
	Filesystem operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
@:headerCode('#include "uv.h"')
abstract File(UvFile) {
	static public final stdin:File = new File(new UvFile(0));
	static public final stdout:File = new File(new UvFile(1));
	static public final stderr:File = new File(new UvFile(2));

	@:allow(cpp.uv)
	var uv(get,never):UvFile;
	inline function get_uv():UvFile
		return this;

	@:allow(cpp.uv)
	inline function new(uv:UvFile)
		this = uv;
}