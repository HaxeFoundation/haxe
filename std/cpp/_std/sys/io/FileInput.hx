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
import sys.io.FileSeek;

@:coreApi
class FileInput extends haxe.io.Input {

	private var __f : Dynamic;

	function new(f:Dynamic) : Void {
		__f = f;
	}

	public override function readByte() : Int {
		return try {
			file_read_char(__f);
		} catch( e : Dynamic ) {
			if( untyped e.__IsArray() )
				throw new haxe.io.Eof();
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function readBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		return try {
			file_read(__f,s.getData(),p,l);
		} catch( e : Dynamic ) {
			if( untyped e.__IsArray() )
				throw new haxe.io.Eof();
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function close() : Void {
		super.close();
		file_close(__f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		file_seek(__f,p,pos==SeekBegin ? 0 : pos==SeekCur ? 1 :  2 );
	}

	public function tell() : Int {
		return file_tell(__f);
	}


	public function eof() : Bool {
		return file_eof(__f);
	}

	private static var file_eof = cpp.Lib.load("std","file_eof",1);

	private static var file_read = cpp.Lib.load("std","file_read",4);
	private static var file_read_char = cpp.Lib.load("std","file_read_char",1);

	private static var file_close = cpp.Lib.load("std","file_close",1);
	private static var file_seek = cpp.Lib.load("std","file_seek",3);
	private static var file_tell = cpp.Lib.load("std","file_tell",1);

}
