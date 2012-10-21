/*
 * Copyright (c) 2005-2007, The haXe Project Contributors
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
import haxe.io.BytesInput;
import haxe.io.Eof;
import java.io.Exceptions;
import java.NativeArray;

@:coreApi
class Process {

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default, null) : haxe.io.Output;

	private var proc:java.lang.Process;

	public function new( cmd : String, args : Array<String> ) : Void
	{
		var pargs = new NativeArray(args.length + 1);
		pargs[0] = cmd;
		for (i in 0...args.length)
		{
			pargs[i + 1] = args[i];
		}

		try
		{
			proc = new java.lang.ProcessBuilder(pargs).start();
		}
		catch (e:Dynamic) { throw e; } //wrapping typed exceptions

		var p = proc;
		stderr = new ProcessInput(p.getErrorStream());
		stdout = new ProcessInput(p.getInputStream());
		stdin = new java.io.NativeOutput(p.getOutputStream());
	}

	public function getPid() : Int
	{
		if (Reflect.hasField(proc, "pid"))
			return Reflect.field(proc, "pid");
		return -1;
	}

	public function exitCode() : Int
	{
		cast(stdout, ProcessInput).bufferContents();
		cast(stderr, ProcessInput).bufferContents();
		try
		{
			proc.waitFor();
		}
		catch (e:Dynamic) { throw e; }
		return proc.exitValue();
	}

	public function close() : Void
	{
		proc.destroy();
	}

	public function kill() : Void
	{
		proc.destroy();
	}

}


private class ProcessInput extends java.io.NativeInput
{
	private var chained:BytesInput;

	public function bufferContents():Void
	{
		if (chained != null) return;
		var b = this.readAll();
		chained = new BytesInput(b);
	}

	override public function readByte():Int
	{
		if (chained != null)
			return chained.readByte();
		try
		{
			return stream.read();
		}
		catch (e:EOFException) {
			throw new Eof();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int
	{
		if (chained != null)
			return chained.readBytes(s, pos, len);

		var ret = -1;
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
		if (chained != null)
			chained.close();
		try
		{
			stream.close();
		}

		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
}