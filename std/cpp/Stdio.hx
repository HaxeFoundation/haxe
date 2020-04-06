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

package cpp;

import haxe.extern.Rest;

@:include("stdio.h")
extern class Stdio {
	@:native("printf")
	static function printf(format:ConstCharStar, rest:Rest<VarArg>):Void;

	@:native("fopen")
	static function fopen(filename:ConstCharStar, mode:ConstCharStar):FILE;

	@:native("fwrite")
	static function fwrite<T>(data:RawPointer<T>, elemSize:SizeT, elemCount:SizeT, file:FILE):SizeT;

	@:native("fclose")
	static function fclose(file:FILE):Int;

	@:native("fprintf")
	static function fprintf(file:FILE, format:ConstCharStar, rest:Rest<VarArg>):Void;
}
