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

typedef ZipEntry = {
	var fileName : String;
	var fileSize : Int;
	var compressedData : String;
}

class File {

	public static function unzip( f : ZipEntry ) : String {
		var c = new Uncompress(-15);
		var s = neko.Lib.makeString(f.fileSize);
		var r = c.run(f.compressedData,0,s,0);
		c.close();
		if( !r.done || r.read != f.compressedData.length || r.write != f.fileSize )
			throw "Invalid compressed data for "+f.fileName;
		return s;
	}

	public static function read( data : neko.io.Input ) : List<ZipEntry> {
		var p = 0;
		var l = new List();
		while( true ) {
			var h = data.readInt32();
			if( h == 0x02014B50 || h == 0x06054B50 )
				break;
			if( h != 0x04034B50 )
				throw "Invalid Zip Data";
			data.skip(18);
			var csize = data.readInt32();
			data.skip(4);
			var usize = data.readInt32();
			data.skip(4);
			var fnamelen = data.readInt16();
			data.skip(2);
			var elen = data.readInt16();
			data.skip(2);
			var fname = data.readBytes(fnamelen);
			data.skip(elen);
			l.add({
				fileName : fname,
				fileSize : usize,
				compressedData : data.readBytes(csize)
			});
		}
		return l;
	}

}