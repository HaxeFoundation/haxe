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

private extern class NativeProcess {
	function new(cmd:String, ?args:Array<String>):Void;

	function close():Void;
	function exitCode():Int;
	function getPid():Int;
	function kill():Void;

	function readStderr(bytes:haxe.io.Bytes, pos:Int, len:Int):Int;
	function readStdout(bytes:haxe.io.Bytes, pos:Int, len:Int):Int;

	function closeStdin():Void;
	function writeStdin(bytes:haxe.io.Bytes, pos:Int, len:Int):Int;
}

private class Stdin extends haxe.io.Output {
	var proc:NativeProcess;
	var buf:haxe.io.Bytes;

	public function new(proc:NativeProcess) {
		this.proc = proc;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function close() {
		super.close();
		proc.closeStdin();
	}

	public override function writeByte(c:Int) {
		buf.set(0, c);
		writeBytes(buf, 0, 1);
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int) {
		try {
			return proc.writeStdin(buf, pos, len);
		} catch (e:Dynamic) {
			throw new haxe.io.Eof();
		}
	}
}

private class Stdout extends haxe.io.Input {
	var proc:NativeProcess;
	var out:Bool;
	var buf:haxe.io.Bytes;

	public function new(proc:NativeProcess, out:Bool) {
		this.proc = proc;
		this.out = out;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function readByte() {
		if (readBytes(buf, 0, 1) == 0)
			throw haxe.io.Error.Blocked;
		return buf.get(0);
	}

	public override function readBytes(bytes:haxe.io.Bytes, pos:Int, len:Int):Int {
		try {
			if (out) {
				return proc.readStdout(bytes, pos, len);
			} else {
				return proc.readStderr(bytes, pos, len);
			}
		} catch (e:Dynamic) {
			throw new haxe.io.Eof();
		}
	}
}

@:coreApi
class Process {
	public var stdout(default, null):haxe.io.Input;
	public var stderr(default, null):haxe.io.Input;
	public var stdin(default, null):haxe.io.Output;

	var proc:NativeProcess;

	public function new(cmd:String, ?args:Array<String>, ?detached:Bool):Void {
		if (detached) {
			throw "Detached process is not supported on this platform";
		}
		proc = new NativeProcess(cmd, args);
		stdout = new Stdout(proc, true);
		stderr = new Stdout(proc, false);
		stdin = new Stdin(proc);
	}

	public inline function getPid():Int {
		return proc.getPid();
	}

	public function exitCode(block:Bool = true):Null<Int> {
		if (block == false)
			throw "Non blocking exitCode() not supported on this platform";
		return proc.exitCode();
	}

	public inline function close():Void {
		proc.close();
	}

	public inline function kill():Void {
		proc.kill();
	}
}
