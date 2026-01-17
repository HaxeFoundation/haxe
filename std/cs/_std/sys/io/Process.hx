/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import haxe.SysTools;
import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.io.Eof;
import haxe.io.Input;
import haxe.io.Output;

@:coreApi
class Process {
	public var stdout(default, null):Input;
	public var stderr(default, null):Input;
	public var stdin(default, null):Output;

	private var proc:Dynamic; // System.Diagnostics.Process

	public function new(cmd:String, ?args:Array<String>, ?detached:Bool):Void {
		if (detached)
			throw "Detached process is not supported on this platform";

		var sysName = Sys.systemName();
		var fileName:String;
		var arguments:String;

		if (args == null) {
			// Shell mode - interpret cmd as a shell command
			if (sysName == "Windows") {
				var comspec = Sys.getEnv("COMSPEC");
				fileName = comspec != null ? comspec : "cmd.exe";
				arguments = '/C "' + cmd + '"';
			} else {
				fileName = "/bin/sh";
				arguments = "-c " + escapeShellArg(cmd);
			}
		} else {
			// Direct mode - cmd is the executable, args are arguments
			if (sysName == "Windows") {
				fileName = cmd;
				var sb = new StringBuf();
				for (i in 0...args.length) {
					if (i > 0)
						sb.add(" ");
					sb.add(SysTools.quoteWinArg(args[i], false));
				}
				arguments = sb.toString();
			} else {
				fileName = cmd;
				var sb = new StringBuf();
				for (i in 0...args.length) {
					if (i > 0)
						sb.add(" ");
					sb.add(escapeShellArg(args[i]));
				}
				arguments = sb.toString();
			}
		}

		proc = untyped __cs__("new System.Diagnostics.Process()");
		untyped __cs__("{0}.StartInfo.FileName = {1}", proc, fileName);
		untyped __cs__("{0}.StartInfo.Arguments = {1}", proc, arguments);
		untyped __cs__("{0}.StartInfo.UseShellExecute = false", proc);
		untyped __cs__("{0}.StartInfo.RedirectStandardInput = true", proc);
		untyped __cs__("{0}.StartInfo.RedirectStandardOutput = true", proc);
		untyped __cs__("{0}.StartInfo.RedirectStandardError = true", proc);
		untyped __cs__("{0}.StartInfo.CreateNoWindow = true", proc);
		untyped __cs__("{0}.Start()", proc);

		var stdoutStream:cs.system.io.Stream = untyped __cs__("{0}.StandardOutput.BaseStream", proc);
		var stderrStream:cs.system.io.Stream = untyped __cs__("{0}.StandardError.BaseStream", proc);
		var stdinStream:cs.system.io.Stream = untyped __cs__("{0}.StandardInput.BaseStream", proc);
		stdout = new ProcessInput(stdoutStream);
		stderr = new ProcessInput(stderrStream);
		stdin = new ProcessOutput(stdinStream);
	}

	private static function escapeShellArg(arg:String):String {
		// Simple escaping for Unix shells
		if (arg.indexOf(" ") >= 0 || arg.indexOf("'") >= 0 || arg.indexOf('"') >= 0) {
			return "'" + StringTools.replace(arg, "'", "'\\''") + "'";
		}
		return arg;
	}

	public function getPid():Int {
		return untyped __cs__("{0}.Id", proc);
	}

	public function exitCode(block:Bool = true):Null<Int> {
		if (!block) {
			var hasExited:Bool = untyped __cs__("{0}.HasExited", proc);
			if (!hasExited)
				return null;
			return untyped __cs__("{0}.ExitCode", proc);
		}

		cast(stdout, ProcessInput).bufferContents();
		cast(stderr, ProcessInput).bufferContents();
		untyped __cs__("{0}.WaitForExit()", proc);
		return untyped __cs__("{0}.ExitCode", proc);
	}

	public function close():Void {
		untyped __cs__("{0}.Close()", proc);
	}

	public function kill():Void {
		untyped __cs__("{0}.Kill()", proc);
	}
}

private class ProcessInput extends Input {
	var stream:cs.system.io.Stream;
	var chained:BytesInput;

	public function new(stream:cs.system.io.Stream) {
		this.stream = stream;
		this.chained = null;
	}

	public function bufferContents():Void {
		if (chained != null)
			return;
		var b = this.readAll();
		chained = new BytesInput(b);
	}

	override public function readByte():Int {
		if (chained != null)
			return chained.readByte();
		var ret:Int = stream.ReadByte();
		if (ret == -1)
			throw new Eof();
		return ret;
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		if (chained != null)
			return chained.readBytes(s, pos, len);

		var ret:Int = untyped __cs__("{0}.Read({1}, {2}, {3})", stream, s.getData(), pos, len);
		if (ret == 0)
			throw new Eof();
		return ret;
	}

	override public function close():Void {
		if (chained != null)
			chained.close();
		stream.Close();
	}
}

private class ProcessOutput extends Output {
	var stream:cs.system.io.Stream;

	public function new(stream:cs.system.io.Stream) {
		this.stream = stream;
	}

	override public function writeByte(c:Int):Void {
		stream.WriteByte(c);
	}

	override public function writeBytes(s:Bytes, pos:Int, len:Int):Int {
		untyped __cs__("{0}.Write({1}, {2}, {3})", stream, s.getData(), pos, len);
		return len;
	}

	override public function close():Void {
		stream.Close();
	}

	override public function flush():Void {
		stream.Flush();
	}
}
