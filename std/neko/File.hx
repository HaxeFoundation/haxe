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
package neko;

/**
	API for reading and writing to files.
**/
class File {

	private var __f : Void;

	private function new(f) {
		__f = f;
	}

	public function name() : String {
		return new String(file_name(__f));
	}

	public function read( nbytes : Int ) : String {
		var s = untyped __dollar__smake(nbytes);
		var p = 0;
		while( nbytes > 0 ) {
			var k = file_read(__f,s,p,nbytes);
			if( k == 0 ) throw "Blocked";
			p += k;
			nbytes -= k;
		}
		return new String(s);
	}

	public function readBuf( s : String, p : Int, l : Int ) : Int {
		return file_read(__f,untyped s.__s,p,l);
	}

	public function readChar() : Int {
		return file_read_char(__f);
	}

	public function write( str : String ) {
		var l = str.length;
		var p = 0;
		while( l > 0 ) {
			var k = file_write(__f,untyped str.__s,p,l);
			if( k == 0 ) throw "Blocked";
			p += k;
			l -= k;
		}
	}

	public function writeSub( s : String, p : Int, l : Int ) : Int {
		return file_write(__f,untyped s.__s,p,l);
	}

	public function writeChar( c : Int ) {
		file_write_char(__f,c);
	}

	public function close() {
		file_close(__f);
	}

	public function seekBegin( p : Int ) : Int {
		return file_seek(__f,p,0);
	}

	public function seekEnd( p : Int ) : Int {
		return file_seek(__f,p,2);
	}

	public function seek( p : Int ) : Int {
		return file_seek(__f,p,1);
	}

	public function tell() : Int {
		return file_tell(__f);
	}

	public function eof() : Bool {
		return file_eof(__f);
	}

	public function flush() {
		file_flush(__f);
	}

	// --- Statics ----

	public static function getContent( filename : String ) {
		return new String(file_contents(untyped filename.__s));
	}

	public static function read( path : String, binary : Bool ) {
		return new File(untyped file_open(path.__s,(if( binary ) "rb" else "r").__s));
	}

	public static function write( path : String, binary : Bool ) {
		return new File(untyped file_open(path.__s,(if( binary ) "wb" else "w").__s));
	}

	public static function append( path : String, binary : Bool ) {
		return new File(untyped file_open(path.__s,(if( binary ) "ab" else "a").__s));
	}

	private static var file_contents = Lib.load("std","file_contents",1);
	private static var file_open = Lib.load("std","file_open",2);
	private static var file_close = Lib.load("std","file_close",1);
	private static var file_name = Lib.load("std","file_name",1);
	private static var file_write = Lib.load("std","file_write",4);
	private static var file_read = Lib.load("std","file_read",4);
	private static var file_read_char = Lib.load("std","file_read_char",1);
	private static var file_write_char = Lib.load("std","file_write_char",2);
	private static var file_seek = Lib.load("std","file_seek",3);
	private static var file_tell = Lib.load("std","file_tell",1);
	private static var file_eof = Lib.load("std","file_eof",1);
	private static var file_flush = Lib.load("std","file_flush",1);

}