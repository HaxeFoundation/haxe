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
package java.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Input;
import java.io.IOException;
import java.io.EOFException;

@:native('haxe.java.io.NativeInput') class NativeInput extends Input
{
	var stream:java.io.InputStream;
	public function new(stream)
	{
		this.stream = stream;
	}

	override public function readByte():Int
	{
		var ret = 0;
		try
		{
			ret = stream.read();
		}
		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
		if ( ret == -1 )
			throw new Eof();
		return ret;
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int
	{
		var ret = 0;
		try
		{
			ret = stream.read(s.getData(), pos, len);
		}

		catch (e:EOFException) {
			throw new Eof();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}

		if (ret == -1)
			throw new Eof();
		return ret;
	}

	override public function close():Void
	{
		try
		{
			stream.close();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
}
