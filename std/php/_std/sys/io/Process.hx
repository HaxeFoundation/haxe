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
import php.NativeArray;

private class Stdin extends haxe.io.Output {
	var p : Void;
	var buf : haxe.io.Bytes;

	public function new(p) {
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
	var p : Void;
	var buf : haxe.io.Bytes;

	public function new(p) {
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
	var p : Void;
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