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
import haxe.io.BytesInput;
import cs.system.io.StreamReader;
import cs.system.io.StreamWriter;
import cs.system.diagnostics.Process as NativeProcess;
import cs.system.diagnostics.ProcessStartInfo as NativeStartInfo;

@:coreApi
class Process {

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default, null) : haxe.io.Output;

	private var native:NativeProcess;


	public function new( cmd : String, args : Array<String> ) : Void
	{
		this.native = new NativeProcess();
		native.StartInfo.FileName = cmd;
		native.StartInfo.CreateNoWindow = true;
		var buf = new StringBuf();
		for (arg in args)
		{
			buf.add("\"");
			buf.add(StringTools.replace(arg, "\"", "\\\""));
			buf.add("\" ");
		}
		native.StartInfo.Arguments = buf.toString();
		native.StartInfo.RedirectStandardError = native.StartInfo.RedirectStandardInput = native.StartInfo.RedirectStandardOutput = true;
		native.StartInfo.UseShellExecute = false;

		native.Start();

		this.stdout = new cs.io.NativeInput(native.StandardOutput.BaseStream);
		this.stderr = new cs.io.NativeInput(native.StandardError.BaseStream);
		this.stdin = new cs.io.NativeOutput(native.StandardInput.BaseStream);
	}

	public function getPid() : Int
	{
		return native.Id;
	}

	public function exitCode() : Int
	{
		native.WaitForExit();
		return native.ExitCode;
	}

	public function close() : Void
	{
		native.Close();
	}

	public function kill() : Void
	{
		native.Kill();
	}

}
