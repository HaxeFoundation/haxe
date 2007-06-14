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
package neko.io;

private class Stdin extends neko.io.Output {

	var p : Void;

	public function new(p) {
		this.p = p;
	}

	public override function close() {
		super.close();
		_stdin_close(p);
	}

	public override function writeChar(c) {
		if( writeBytes(Std.chr(c),0,1) == 0 )
			throw Error.Blocked;
	}

	public override function writeBytes( str : String, pos : Int, len : Int ) : Int {
		try {
			return _stdin_write(p,untyped str.__s,pos,len);
		} catch( e : Dynamic ) {
			throw new Eof();
		}
	}

	static var _stdin_write = neko.Lib.load("std","process_stdin_write",4);
	static var _stdin_close = neko.Lib.load("std","process_stdin_close",1);

}

private class Stdout extends neko.io.Input {

	var p : Void;
	var out : Bool;
	var buf : String;

	public function new(p,out) {
		this.p = p;
		this.out = out;
		buf = neko.Lib.makeString(1);
	}

	public override function readChar() {
		if( readBytes(buf,0,1) == 0 )
			throw Error.Blocked;
		return buf.charCodeAt(0);
	}

	public override function readBytes( str : String, pos : Int, len : Int ) : Int {
		try {
			return (out?_stdout_read:_stderr_read)(p,untyped str.__s,pos,len);
		} catch( e : Dynamic ) {
			throw new Eof();
		}
	}

	static var _stdout_read = neko.Lib.load("std","process_stdout_read",4);
	static var _stderr_read = neko.Lib.load("std","process_stderr_read",4);

}

class Process {

	var p : Void;
	public var stdout(default,null) : neko.io.Input;
	public var stderr(default,null) : neko.io.Input;
	public var stdin(default,null) : neko.io.Output;

	public function new( cmd : String, args : Array<String> ) {
		p = try _run(untyped cmd.__s,neko.Lib.haxeToNeko(args)) catch( e : Dynamic ) throw "Process creation failure : "+cmd;
		stdin = new Stdin(p);
		stdout = new Stdout(p,true);
		stderr = new Stdout(p,false);
	}

	public function exitCode() : Int {
		return _exit(p);
	}

	static var _run = neko.Lib.load("std","process_run",2);
	static var _exit = neko.Lib.load("std","process_exit",1);

}