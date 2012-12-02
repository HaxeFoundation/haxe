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
import php.NativeArray;

private class Stdin extends haxe.io.Output {
	var p : Dynamic;
	var buf : haxe.io.Bytes;

	public function new(p:Dynamic) {
		this.p = p;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function close() {
		super.close();
		untyped __call__('fclose', p);
	}

	public override function writeByte(c) {
		buf.set(0,c);
		writeBytes(buf,0,1);
	}

	public override function writeBytes( b : haxe.io.Bytes, pos : Int, l : Int ) : Int {
		var s = b.readString(pos, l);
		if(untyped __call__('feof', p)) return throw new haxe.io.Eof();
		var r = untyped __call__('fwrite', p, s, l);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return r;
	}
}

private class Stdout extends haxe.io.Input {
	var p : Dynamic;
	var buf : haxe.io.Bytes;

	public function new(p:Dynamic) {
		this.p = p;
		buf = haxe.io.Bytes.alloc(1);
	}

	public override function readByte() {
		if( readBytes(buf,0,1) == 0 )
			throw haxe.io.Error.Blocked;
		return buf.get(0);
	}

	public override function readBytes( str : haxe.io.Bytes, pos : Int, l : Int ) : Int {
		if(untyped __call__('feof', p)) return throw new haxe.io.Eof();
		var r : String = untyped __call__('fread', p, l);
		if(untyped __physeq__(r, "")) return throw new haxe.io.Eof();
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		var b = haxe.io.Bytes.ofString(r);
		str.blit(pos, b, 0, r.length);
		return r.length;
	}
}

@:coreApi
class Process {
	var p : Dynamic;
	var st : NativeArray;
	var cl : Int;
	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	public function new( cmd : String, args : Array<String> ) : Void {
		var pipes = untyped __call__("array");
		var descriptorspec = untyped __php__("array(
			array('pipe', 'r'),
			array('pipe', 'w'),
			array('pipe', 'w')
		)");
		p = untyped __call__('proc_open', cmd+sargs(args), descriptorspec, pipes);
		if(untyped __physeq__(p, false)) throw "Process creation failure : "+cmd;
		stdin  = new Stdin( pipes[0]);
		stdout = new Stdout(pipes[1]);
		stderr = new Stdout(pipes[2]);
	}

	public function close() : Void {
		if(null == st)
			st = untyped __call__('proc_get_status', p);
		replaceStream(stderr);
		replaceStream(stdout);
		cl = untyped __call__('proc_close', p);
	}

	function sargs(args : Array<String>) : String {
		var b = '';
		for(arg in args) {
			arg = arg.split('"').join('\"');
			if(arg.indexOf(' ') >= 0)
				arg = '"'+arg+'"';
			b += ' '+arg;
		}
		return b;
	}

	public function getPid() : Int {
		var r = untyped __call__('proc_get_status', p);
		return r[untyped 'pid'];
	}

	public function kill() : Void {
		untyped __call__('proc_terminate',p);
	}

	function replaceStream(input : haxe.io.Input) : Void {
		var fp = untyped __call__("fopen", "php://memory", "r+");
		while(true) {
			var s = untyped __call__("fread", untyped input.p, 8192);
			if(untyped __physeq__(s, false) || s == null || s == '') break;
			untyped __call__("fwrite", fp, s);
		}
		untyped __call__("rewind", fp);
		untyped input.p = fp;
	}

	public function exitCode() : Int {
		if (null == cl)
		{
			st = untyped __call__('proc_get_status', p);
			while(st[untyped 'running']) {
				Sys.sleep(0.01);
				st = untyped __call__('proc_get_status', p);
			}
			close();
		}
		return (cast st[untyped 'exitcode']) < 0 ? cl : cast st[untyped 'exitcode'];

	}
}