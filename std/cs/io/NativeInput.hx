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
package cs.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Error;
import haxe.io.Input;

class NativeInput extends Input
{
	public var canSeek(get,never):Bool;

	var stream:cs.system.io.Stream;
	var _eof:Bool;

	public function new(stream)
	{
		this.stream = stream;
		this._eof = false;
		if (!stream.CanRead) throw "Write-only stream";
	}

	override public function readByte():Int
	{
		var ret = stream.ReadByte();
		if (ret == -1) {
			_eof = true;
			throw new Eof();
		}
		return ret;
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int
	{
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		var ret = stream.Read(s.getData(), pos, len);
		if (ret == 0) {
			_eof = true;
			throw new Eof();
		}
		return ret;
	}

	override public function close():Void
	{
		stream.Close();
	}

	private inline function get_canSeek():Bool
	{
		return stream.CanSeek;
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		_eof = false;
		var pos = switch(pos)
		{
			case SeekBegin: cs.system.io.SeekOrigin.Begin;
			case SeekCur: cs.system.io.SeekOrigin.Current;
			case SeekEnd: cs.system.io.SeekOrigin.End;
		};

		stream.Seek(cast(p, Int64), pos);
	}

	public function tell() : Int
	{
		return cast(stream.Position, Int);
	}

	public inline function eof() : Bool
	{
		return _eof;
	}
}