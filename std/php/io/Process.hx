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
package php.io;

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
		var r : String = untyped __call__('fread', p, 1);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		var b = haxe.io.Bytes.ofString(r);
		str.blit(pos, b, 0, r.length);
		return r.length;
	}
}

class Process {
	var p : Void;
	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	public function new( cmd : String, args : Array<String> ) {
		var pipes = new Array<Dynamic>();
		var descriptorspec = [
			['pipe', 'r'],
			['pipe', 'w'],
			['pipe', 'w']
		];
		// TODO: check how args are passed in neko 
		p = untyped __call__('proc_open', cmd, descriptorspec, pipes, null, __php__("array('args' => join(' ', $args))"));
		if(untyped __physeq__(p, false)) throw "Process creation failure : "+cmd;
		stdin  = new Stdin(pipes[0]);
		stdout = new Stdout(pipes[1]);
		stderr = new Stdout(pipes[2]);
	}

	public function getPid() : Int {
		return untyped __call__('proc_get_status', p)['pid'];
	}

	public function exitCode() : Int {
		return untyped __call__('proc_get_status', p)['exitcode'];
	}
}