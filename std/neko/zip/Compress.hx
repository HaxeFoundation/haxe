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
package neko.zip;

class Compress {

	var s : Void;

	public function new( level : Int ) {
		s = _deflate_init(level);
	}

	public function run( src : String, srcPos : Int, dst : String, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return _deflate_buffer(s,untyped src.__s,srcPos,untyped dst.__s,dstPos);
	}

	public function setFlushMode( f : Flush ) {
		_set_flush_mode(s,untyped Std.string(f).__s);
	}

	public function close() {
		_deflate_end(s);
	}

	public static function run( s : String, level : Int ) : String {
		var c = new Compress(level);
		c.setFlushMode(Flush.FINISH);
		var out = neko.Lib.makeString(_deflate_bound(c.s,s.length));
		var r = c.run(s,0,out,0);
		c.close();
		if( !r.done || r.read != s.length )
			throw "Compression failed";
		return out.substr(0,r.write);
	}

	static var _deflate_init = neko.Lib.load("zlib","deflate_init",1);
	static var _deflate_bound = neko.Lib.load("zlib","deflate_bound",2);
	static var _deflate_buffer = neko.Lib.load("zlib","deflate_buffer",5);
	static var _deflate_end = neko.Lib.load("zlib","deflate_end",1);
	static var _set_flush_mode = neko.Lib.load("zlib","set_flush_mode",2);

}
