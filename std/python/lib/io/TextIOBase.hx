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
package python.lib.io;

import haxe.extern.EitherType;
import python.lib.io.BufferedIOBase;
import python.lib.io.IOBase;

@:pythonImport("io", "TextIOBase")
extern class TextIOBase extends IOBase implements ITextIOBase {

	public var encoding:String;
	public var error:String;
	public var newlines:Null<EitherType<String, Tuple<String>>>;

	public function detach ():BufferedIOBase;

	public function write (s:String):Int;

	public function read (n:Int):String;

	public var buffer:BufferedIOBase;

}

@:remove extern interface ITextIOBase extends IIOBase {

	public var encoding:String;
	public var error:String;
	public var newlines:Null<EitherType<String, Tuple<String>>>;

	public var buffer:BufferedIOBase;

	public function detach ():BufferedIOBase;

	public function write (s:String):Int;

	public function read (n:Int):String;




}
