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

signature ZipEntry {
	var fileName : String;
	var fileSize : Int;
	var compressedSize : Int;
	var compressedDataPos : Int;
}

class File {

	static function int32( data : String, p : Int ) {
		var c1 = data.charCodeAt(p);
		var c2 = data.charCodeAt(p+1);
		var c3 = data.charCodeAt(p+2);
		var c4 = data.charCodeAt(p+3);
		return c1 | (c2 << 8) | (c3 << 16) | (c4 << 24);
	}

	static function int16( data : String, p : Int ) {
		var c1 = data.charCodeAt(p);
		var c2 = data.charCodeAt(p+1);
		return c1 | (c2 << 8);
	}

	public static function unzip( data : String, f : ZipEntry ) : String {
		var c = new Uncompress(-15);
		var s = neko.Lib.makeString(f.fileSize);
		var r = c.run(data,f.compressedDataPos,s,0);
		c.close();
		if( !r.done || r.read != f.compressedSize || r.write != f.fileSize )
			throw "Invalid compressed data for "+f.fileName;
		return s;
	}

	public static function read( data : String ) : List<ZipEntry> {
		var p = 0;
		var l = new List();
		while( true ) {
			var h = int32(data,p);
			if( h == 0x02014B50 || h == 0x06054B50 )
				break;
			if( h != 0x04034B50 )
				throw "Invalid Zip Data";
			p += 18;
			var csize = int32(data,p);
			p += 4;
			var usize = int32(data,p);
			p += 4;
			var fnamelen = int16(data,p);
			p += 2;
			var elen = int16(data,p);
			p += 2;
			var fname = data.substr(p,fnamelen);
			p += fnamelen;
			p += elen;
			l.add({
				fileName : fname,
				fileSize : usize,
				compressedSize : csize,
				compressedDataPos : p
			});
			p += csize;
		}
		return l;
	}

}