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
		try {
			return _stdin_write(p,buf.getData(),pos,len);
		} catch( e : Dynamic ) {
			throw new haxe.io.Eof();
		}
		return 0;
	}

	static var _stdin_write = cpp.Lib.load("std","process_stdin_write",4);
	static var _stdin_close = cpp.Lib.load("std","process_stdin_close",1);

}

private class Stdout extends haxe.io.Input {

	var p : Dynamic;
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
		var result:Int;
		try {
			result = (out?_stdout_read:_stderr_read)(p,str.getData(),pos,len);
		} catch( e : Dynamic ) {
			throw new haxe.io.Eof();
		}
		if (result==0)throw new haxe.io.Eof();
		return result;
	}

	static var _stdout_read = cpp.Lib.load("std","process_stdout_read",4);
	static var _stderr_read = cpp.Lib.load("std","process_stderr_read",4);

}

@:coreApi
class Process {

	var p : Dynamic;
	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	public function new( cmd : String, args : Array<String> ) : Void {
		p = try _run(cmd,args) catch( e : Dynamic ) throw "Process creation failure : "+cmd;
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
		throw "Not implemented";
	}

	static var _run = cpp.Lib.load("std","process_run",2);
	static var _exit = cpp.Lib.load("std","process_exit",1);
	static var _pid = cpp.Lib.load("std","process_pid",1);
	static var _close = cpp.Lib.loadLazy("std","process_close",1);

}
