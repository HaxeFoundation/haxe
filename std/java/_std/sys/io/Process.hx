/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import haxe.io.BytesInput;
import haxe.io.Eof;
import java.io.IOException;
import java.io.EOFException;
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
