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

import php7.*;
import haxe.io.*;

private class Stdin extends Output {
	var p : Resource;
	var buf : Bytes;

	public function new(p:Resource) {
		this.p = p;
		buf = Bytes.alloc(1);
	}

	public override function close() {
		super.close();
		Global.fclose(p);
	}

	public override function writeByte(c) {
		buf.set(0,c);
		writeBytes(buf,0,1);
	}

	public override function writeBytes( b : Bytes, pos : Int, l : Int ) : Int {
		var s = b.getString(pos, l);
		if(Global.feof(p)) return throw new Eof();
		var r = Global.fwrite(p, s, l);
		if(r == false) return throw Error.Custom('An error occurred');
		return r;
	}
}

private class Stdout extends Input {
	var p : Resource;
	var buf : Bytes;

	public function new(p:Resource) {
		this.p = p;
		buf = Bytes.alloc(1);
	}

	public override function readByte() {
		if( readBytes(buf,0,1) == 0 )
			throw Error.Blocked;
		return buf.get(0);
	}

	public override function readBytes( str : Bytes, pos : Int, l : Int ) : Int {
		if(Global.feof(p)) return throw new Eof();
		var r = Global.fread(p, l);
		if(r == "") return throw new Eof();
		if(r == false) return throw Error.Custom('An error occurred');
		var r:String = r;
		var b = Bytes.ofString(r);
		str.blit(pos, b, 0, r.length);
		return r.length;
	}
}

@:coreApi
class Process {
	var p : Dynamic;
	var st : NativeArray;
	var cl : Int;
	public var stdout(default,null) : Input;
	public var stderr(default,null) : Input;
	public var stdin(default,null) : Output;

	public function new( cmd : String, ?args : Array<String> ) : Void {
		var pipes = Syntax.arrayDecl();
		var descriptorspec = Syntax.arrayDecl(
			Syntax.arrayDecl('pipe', 'r'),
			Syntax.arrayDecl('pipe', 'w'),
			Syntax.arrayDecl('pipe', 'w')
		);
		if (args != null) {
			switch (Sys.systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
							StringTools.quoteWinArg(a, true)
					].join(" ");
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
			}
		}
		p = Global.proc_open(cmd, descriptorspec, pipes);
		if(p == false) throw "Process creation failure : "+cmd;
		stdin  = new Stdin(pipes[0]);
		stdout = new Stdout(pipes[1]);
		stderr = new Stdout(pipes[2]);
	}

	public function close() : Void {
		if(null == st)
			st = Global.proc_get_status(p);
		replaceStream(cast stderr);
		replaceStream(cast stdout);
		if(null == cl)
			cl = Global.proc_close(p);
	}

	public function getPid() : Int {
		var r = Global.proc_get_status(p);
		return r['pid'];
	}

	public function kill() : Void {
		Global.proc_terminate(p);
	}

	function replaceStream(input : {p:Resource}) : Void {
		var fp = Global.fopen("php://memory", "r+");
		while(true) {
			var s = Global.fread(input.p, 8192);
			if(s == false || s == null || s == '') break;
			Global.fwrite(p, s);
		}
		Global.rewind(p);
		input.p = fp;
	}

	public function exitCode() : Int {
		if (null == cl)
		{
			st = Global.proc_get_status(p);
			while(st['running']) {
				Sys.sleep(0.01);
				st = Global.proc_get_status(p);
			}
			close();
		}
		return (cast st['exitcode']) < 0 ? cl : cast st['exitcode'];

	}
}