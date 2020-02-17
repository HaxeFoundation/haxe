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

import lua.Io;
import lua.lib.luv.Pipe;
import lua.lib.luv.Signal;
import lua.lib.luv.Loop;
import lua.Boot;
import lua.Table;
import lua.NativeStringTools;
import haxe.SysTools;
import haxe.io.Bytes;
import haxe.io.Error;
import haxe.io.Eof;

@:coreApi
class Process {
	var _pid:Int;
	var _handle:lua.lib.luv.Process;
	var _code:Int;
	var closef:Int->Signal->Void;

	public var stdout(default, null):haxe.io.Input;
	public var stderr(default, null):haxe.io.Input;
	public var stdin(default, null):haxe.io.Output;

	static var argQuote = Sys.systemName() == "Windows" ? function(x) return SysTools.quoteWinArg(x, true) : SysTools.quoteUnixArg;
	static var _shell = Sys.systemName() == "Windows" ? 'cmd.exe' : '/bin/sh';

	/**
		Sets the args for the shell, which will include the cmd to be executed
		by the shell.
	**/
	static function setArgs(cmd:String, ?args:Array<String>):Table<Int, String> {
		var pargs = lua.Table.create();
		var idx = 1;
		if (sys.FileSystem.exists(cmd))
			cmd = '"$cmd"'; // escape simple paths
		var all = [cmd];
		if (args != null) {
			for (a in args) {
				all.push(argQuote(a));
			}
		}

		if (Sys.systemName() == "Windows") {
			pargs[idx++] = '/s';
			pargs[idx++] = '/c';
			pargs[idx++] = all.join(" ");
		} else {
			pargs[idx++] = "-c";
			pargs[idx++] = all.join(" ");
		}
		return pargs;
	}

	public function new(cmd:String, ?args:Array<String>, ?detached:Bool) {
		if (detached)
			throw "Detached process is not supported on this platform";

		var _stdout = new Pipe(false);
		var _stderr = new Pipe(false);
		var _stdin = new Pipe(false);
		stdout = new ProcessInput(_stdout);
		stderr = new ProcessInput(_stderr);
		stdin = new ProcessOutput(_stdin);
		var stdio = untyped __lua_table__([_stdin, _stdout, _stderr]);

		var opt = {args: setArgs(cmd, args), stdio: stdio};

		var p = lua.lib.luv.Process.spawn(_shell, opt, function(code:Int, signal:Signal) {
			_code = code;
			if (!_handle.is_closing()){
			    _handle.close();
            }
            _stdin.shutdown(()->_stdin.close());
            _stderr.shutdown(()->_stderr.close());
            _stdout.shutdown(()->_stdout.close());

		});
		_handle = p.handle;

		if (p.handle == null)
			throw p.pid;

		_pid = p.pid;
	}

	public function getPid():Int {
		return _pid;
	}

	public function close():Void {
		if (!_handle.is_closing()){
            _handle.close();
        }
	}

	public function exitCode(block:Bool = true):Null<Int> {
		if (!block)
			return _code;
		while (_handle.is_active()) {
			Loop.run(); // process io until the handle closes (emulate blocking)
		}
		return _code;
	}

	public function kill():Void {
		_handle.kill("sigterm");
	}
}

private class ProcessInput extends haxe.io.Input {
	var b:Pipe;
	var buf:String;
	var idx:Int;
	var _eof:Bool;

	public function new(pipe:Pipe) {
		b = pipe;
		_eof = false;
	}

	inline public function eof():Bool {
		return _eof;
	}

	override function readBytes(s:Bytes, pos:Int, len:Int):Int {
		if (eof())
			throw new haxe.io.Eof();
		return super.readBytes(s, pos, len);
	}

	override public function readByte() {
		var err_str = null;
		if (buf == null || idx >= NativeStringTools.len(buf)) {
			buf = null;
			idx = 0;
			var pending = true;
			b.read_start(function(err, chunk) {
				if (chunk != null) {
					if (buf != null) {
						buf = buf + chunk;
					} else {
						buf = chunk;
					}
				}
				if (err != null)
					err_str = err;
				pending = false;
			});
			// process io until we read our input (emulate blocking)
			while (pending)
				Loop.run();
		}
		if (buf == null) {
			_eof = true;
			throw new haxe.io.Eof();
		}
		if (err_str != null)
			throw err_str;
		var code = NativeStringTools.byte(buf, ++idx);
		return code;
	}

	override public function readAll(?bufsize:Int):Bytes {
		if (bufsize == null)
			bufsize = (1 << 14); // 16 Ko

		var buf = Bytes.alloc(bufsize);
		var total = new haxe.io.BytesBuffer();
		try {
			while (true) {
				var len = readBytes(buf, 0, bufsize);
				// don't throw blocked error here
				if (len != 0)
					total.addBytes(buf, 0, len);
				if (len < bufsize)
					break;
			}
		} catch (e:Eof) {
			_eof = true;
		}
		return total.getBytes();
	}

	override public function close() {
		b.close();
	}
}

private class ProcessOutput extends haxe.io.Output {
	var b:Pipe;

	public function new(pipe:Pipe) {
		b = pipe;
		set_bigEndian(Boot.platformBigEndian);
	}

	override public function writeByte(c:Int):Void {
		b.write(NativeStringTools.char(c));
	}

	override public function close() {
		b.close();
	}
}
