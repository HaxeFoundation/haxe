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
import haxe.io.Output;

class NativeOutput extends Output
{
	var canSeek(get,never):Bool;

	var stream:cs.system.io.Stream;
	public function new(stream)
	{
		this.stream = stream;
		if (!stream.CanWrite) throw "Read-only stream";
	}

	override public function writeByte(c:Int):Void
	{
		stream.WriteByte(cast c);
	}

	override public function close():Void
	{
		stream.Close();
	}

	override public function flush():Void
	{
		stream.Flush();
	}

	override public function prepare(nbytes:Int):Void
	{
		//TODO see if implementation is correct
		stream.SetLength(haxe.Int64.add(stream.Length, cast(nbytes, Int64)));
	}

	private inline function get_canSeek():Bool
	{
		return stream.CanSeek;
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		var p = switch(pos)
		{
			case SeekBegin: cs.system.io.SeekOrigin.Begin;
			case SeekCur: cs.system.io.SeekOrigin.Current;
			case SeekEnd: cs.system.io.SeekOrigin.End;
		};

		stream.Seek(cast(p, Int64), p);
	}

	public function tell() : Int
	{
		return cast(stream.Position, Int);
	}
}
