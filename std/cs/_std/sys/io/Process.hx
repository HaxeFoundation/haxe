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
import haxe.io.BytesInput;
import cs.system.io.StreamReader;
import cs.system.io.StreamWriter;

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

/*
FIXME: The actual process class is much more complex than this, so here it is included a very simplified version.
*/
@:native('System.Diagnostics.Process') private extern class NativeProcess
{
	var ExitCode(default, null):Int;
	var Id(default, null):Int;
	var StartInfo(default, null):NativeStartInfo;
	var StandardError(default, null):StreamReader;
	var StandardInput(default, null):StreamWriter;
	var StandardOutput(default, null):StreamReader;

	function new():Void;
	function Close():Void;
	function Kill():Void;
	function Start():Void;
	function WaitForExit():Void;
}

@:native('System.Diagnostics.ProcessStartInfo') private extern class NativeStartInfo
{
	var Arguments:String;
	var FileName:String;
	var RedirectStandardError:Bool;
	var RedirectStandardInput:Bool;
	var RedirectStandardOutput:Bool;
	var UseShellExecute:Bool;
	function new(filename:String, args:String):Void;

}