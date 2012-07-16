/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package sys.io;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Output;

import java.io.Exceptions;

/**
	Use [sys.io.File.write] to create a [FileOutput]
**/
class FileOutput extends Output {
	var f:java.io.RandomAccessFile;
	public function new(f)
	{
		this.f = f;
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
