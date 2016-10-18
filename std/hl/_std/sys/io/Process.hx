/*
 * Copyright (C)2005-2016 Haxe Foundation
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

private typedef ProcessHandle = hl.types.NativeAbstract<"hl_process">;

private class Stdin extends haxe.io.Output {

	var p : Dynamic;
	var buf : haxe.io.Bytes;

	public function new(p) {
		this.p = p;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function close() {
		super.close();
		_stdin_close(p);
	}

	public override function writeByte(c) {
		buf.set(0,c);
		writeBytes(buf,0,1);
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		var v = _stdin_write(p, buf.getData().bytes, pos, len);
		if( v < 0 ) throw new haxe.io.Eof();
		return v;
	}

	@:hlNative("std","process_stdin_write") static function _stdin_write( p : ProcessHandle, bytes : hl.types.Bytes, pos : Int, len : Int ) : Int { return 0; }
	@:hlNative("std","process_stdin_close") static function _stdin_close( p : ProcessHandle ) : Void { }

}

private class Stdout extends haxe.io.Input {

	var p : ProcessHandle;
	var out : Bool;
	var buf : haxe.io.Bytes;

	public function new(p,out) {
		this.p = p;
		this.out = out;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function readByte() {
		if( readBytes(buf,0,1) == 0 )
			throw haxe.io.Error.Blocked;
		return buf.get(0);
	}

	public override function readBytes( str : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		var v = out ? _stdout_read(p,str.getData().bytes,pos,len) : _stderr_read(p,str.getData().bytes,pos,len);
		if( v < 0 ) throw new haxe.io.Eof();
		return v;
	}

	@:hlNative("std","process_stdout_read") static function _stdout_read( p : ProcessHandle, bytes : hl.types.Bytes, pos : Int, len : Int ) : Int { return 0; }
	@:hlNative("std","process_stderr_read") static function _stderr_read( p : ProcessHandle, bytes : hl.types.Bytes, pos : Int, len : Int ) : Int { return 0; }

}

@:access(Sys)
@:coreApi class Process {

	var p : ProcessHandle;
	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	public function new( cmd : String, ?args : Array<String> ) : Void {
		@:privateAccess {
			var aargs = null;
			if( args != null ) {
				aargs = new hl.types.NativeArray<hl.types.Bytes>(args.length);
				for( i in 0...args.length )
					aargs[i] = Sys.getPath(args[i]);
			}
			p = _run(Sys.getPath(cmd), aargs);
		}
		if( p == null )
			throw new Sys.SysError("Process creation failure : "+cmd);
		stdin = new Stdin(p);
		stdout = new Stdout(p,true);
		stderr = new Stdout(p,false);
	}

	public function getPid() : Int {
		return _pid(p);
	}

	public function exitCode() : Int {
		return _exit(p);
	}

	public function close() : Void {
		_close(p);
	}

	public function kill() : Void {
		_kill(p);
	}

	@:hlNative("std","process_run")	static function _run( cmd : hl.types.Bytes, args : hl.types.NativeArray<hl.types.Bytes> ) : ProcessHandle { return null; }
	@:hlNative("std", "process_exit") static function _exit( p : ProcessHandle ) : Int { return 0; }
	@:hlNative("std", "process_pid") static function _pid( p : ProcessHandle ) : Int { return 0; }
	@:hlNative("std","process_close") static function _close( p : ProcessHandle ) : Void { }
	@:hlNative("std","process_kill") static function _kill( p : ProcessHandle ) : Void { }

}