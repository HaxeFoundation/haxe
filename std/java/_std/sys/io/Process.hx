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
	@:allow(Sys)
	private static function createProcessBuilder(cmd:String, ?args:Array<String>):java.lang.ProcessBuilder {
		var sysName = Sys.systemName();
		var pargs;
		if (args == null) {
			var cmdStr = cmd;
			switch (sysName) {
				case "Windows":
					pargs = new NativeArray(3);
					pargs[0] = cmd = switch (Sys.getEnv("COMSPEC")) {
						case null: "cmd.exe";
						case comspec: comspec;
					}
					pargs[1] = '/C';
					pargs[2] = '"$cmdStr"';
				case _:
					pargs = new NativeArray(3);
					pargs[0] = cmd = "/bin/sh";
					pargs[1] = "-c";
					pargs[2] = cmdStr;
			}
		} else {
			pargs = new NativeArray(args.length + 1);
			switch (sysName) {
				case "Windows":
					pargs[0] = StringTools.quoteWinArg(cmd, false);
					for (i in 0...args.length)
					{
						pargs[i + 1] = StringTools.quoteWinArg(args[i], false);
					}
				case _:
					pargs[0] = cmd;
					for (i in 0...args.length)
					{
						pargs[i + 1] = args[i];
					}
			}
		}

		return new java.lang.ProcessBuilder(pargs);
	}

	public function new( cmd : String, ?args : Array<String> ) : Void
	{
		var p = proc = createProcessBuilder(cmd, args).start();
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

	public function exitCode( block : Bool = true ) : Null<Int>
	{
		if( block == false ) {
			try {
				return proc.exitValue();
			} catch( e : Dynamic ) {
				return null;
			}
		}
		
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
