/*
 * Copyright (c) 2005, The haXe Project Contributors
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

@:coreApi
class FileOutput extends haxe.io.Output {
	private var __f : File.FileHandle;

	function new(f:File.FileHandle) : Void {
		__f = f;
	}

	public override function writeByte( c : Int ) : Void {
		var r = untyped __call__('fwrite', __f, __call__('chr', c));
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public override function writeBytes( b : haxe.io.Bytes, p : Int, l : Int ) : Int {
		var s = b.readString(p, l);
		if(untyped __call__('feof', __f)) return throw new haxe.io.Eof();
		var r = untyped __call__('fwrite', __f, s, l);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return r;
	}

	public override function flush() : Void {
		var r = untyped __call__('fflush', __f);
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public override function close() : Void {
		super.close();
		if(__f != null)	untyped __call__('fclose', __f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		var w;
		switch( pos ) {
			case SeekBegin: w = untyped __php__('SEEK_SET');
			case SeekCur  : w = untyped __php__('SEEK_CUR');
			case SeekEnd  : w = untyped __php__('SEEK_END');
		}
		var r = untyped __call__('fseek', __f, p, w);
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public function tell() : Int {
		var r = untyped __call__('ftell', __f);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return cast r;
	}

}
