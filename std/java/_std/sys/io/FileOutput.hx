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
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Output;
import java.io.EOFException;
import java.io.IOException;

class FileOutput extends Output {
	var f:java.io.RandomAccessFile;
	public function new(f)
	{
		this.f = f;
	}

	override public function close()
	{
		try f.close() catch(e:Dynamic) throw e;
	}

	override public function writeByte(c:Int):Void
	{
		try
		{
			this.f.write(c);
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	override public function write(s:Bytes):Void
	{
		try
		{
			this.f.write(s.getData());
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	override public function writeBytes(s:Bytes, pos:Int, len:Int):Int
	{
		try
		{
			this.f.write(s.getData(), pos, len);
			return len;
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	public function seek( p : Int, pos : FileSeek ) : Void
	{
		try
		{
			switch(pos)
			{
				case SeekBegin: f.seek(cast p);
				case SeekCur: f.seek(haxe.Int64.add(f.getFilePointer(), cast(p, haxe.Int64)));
				case SeekEnd: f.seek(haxe.Int64.add(f.length(), cast p));
			}
		}
		catch (e:EOFException) {
			throw new Eof();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	public function tell() : Int
	{
		try
		{
			return cast f.getFilePointer();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
}
