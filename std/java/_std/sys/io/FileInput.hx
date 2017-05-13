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
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Input;
import java.io.EOFException;
import java.io.IOException;

class FileInput extends Input {
	var f:java.io.RandomAccessFile;
	var _eof:Bool;
	public function new(f)
	{
		this.f = f;
		this._eof = false;
	}

	override public function close()
	{
		try f.close() catch(e:Dynamic) throw e;
	}

	override public function readByte():Int
	{
		try
		{
			return f.readUnsignedByte();
		}

		catch (e:EOFException) {
			_eof = true;
			throw new Eof();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int
	{
		var ret = 0;
		try
		{
			ret = f.read(s.getData(), pos, len);
		}

		catch (e:EOFException) {
			_eof = true;
			throw new Eof();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}

		if (ret == -1) {
			_eof = true;
			throw new Eof();
		}

		return ret;
	}

	public function seek( p : Int, pos : FileSeek ) : Void
	{
		_eof = false;
		try
		{
			switch(pos)
			{
				case SeekBegin: f.seek(cast p);
				case SeekCur: f.seek(haxe.Int64.add(f.getFilePointer(), cast(p, Int64)));
				case SeekEnd: f.seek(haxe.Int64.add(f.length(), cast p));
			}
		}

		catch (e:EOFException) {
			_eof = true;
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

	public inline function eof() : Bool
	{
		return _eof;
	}
}
