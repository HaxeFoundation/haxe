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
	var compressed : Bool;
	var compressedSize : Int;
	var data : String;
}

class File {

	public static function unzip( f : ZipEntry ) : String {
		if( !f.compressed )
			return f.data;
		var c = new Uncompress(-15);
		var s = neko.Lib.makeString(f.fileSize);
		var r = c.run(f.data,0,s,0);
		c.close();
		if( !r.done || r.read != f.data.length || r.write != f.fileSize )
			throw "Invalid compressed data for "+f.fileName;
		return s;
	}

	public static function readZipEntry( i : neko.io.Input ) : ZipEntry {
		var h = i.readInt32();
		if( h == 0x02014B50 || h == 0x06054B50 )
			return null;
		if( h != 0x04034B50 )
			throw "Invalid Zip Data";
		var version = i.readUInt16();
		var flags = i.readUInt16();
		if( flags != 0 )
			throw "Unsupported flags "+flags;
		var compression = i.readUInt16();
		var compressed = (compression != 0);
		if( compressed && compression != 8 )
			throw "Unsupported compression "+compression;
		var lastmodTime = i.readUInt16();
		var lastmodDate = i.readUInt16();
		var crc32 = i.read(4);
		var csize = i.readInt32();
		var usize = i.readInt32();
		var fnamelen = i.readInt16();
		var elen = i.readInt16();
		var fname = i.read(fnamelen);
		var ename = i.read(elen);
		return {
			fileName : fname,
			fileSize : usize,
			compressed : compressed,
			compressedSize : csize,
			data : null,
		};
	}

	public static function readZip( i : neko.io.Input ) : List<ZipEntry> {
		var l = new List();
		while( true ) {
			var e = readZipEntry(i);
			if( e == null )
				break;
			e.data = i.read(e.compressedSize);
			l.add(e);
		}
		return l;
	}

	public static function readTar( i : neko.io.Input, ?gz : Bool ) : List<ZipEntry> {
		if( gz ) {
			var tmp = new neko.io.StringOutput();
			readGZHeader(i);
			readGZData(i,tmp);
			i = new neko.io.StringInput(tmp.toString());
		}
		var l = new List();
		while( true ) {
			var e = readTarEntry(i);
			if( e == null )
				break;
			var pad = Math.ceil(e.fileSize / 512) * 512 - e.fileSize;
			var data = i.read(e.fileSize);
			i.read(pad);
			l.add({
				fileName : e.fileName,
				fileSize : e.fileSize,
				compressed : false,
				compressedSize : e.fileSize,
				data : data,
			});
		}
		return l;
	}

	public static function readGZHeader( i : neko.io.Input ) : String {
		if( i.readChar() != 0x1F || i.readChar() != 0x8B )
			throw "Invalid GZ header";
		if( i.readChar() != 8 )
			throw "Invalid compression method";
		var flags = i.readChar();
		var mtime = i.read(4);
		var xflags = i.readChar();
		var os = i.readChar();
		var fname = null;
		var comments = null;
		if( flags & 4 != 0 ) {
			var xlen = i.readUInt16();
			var xdata = i.read(xlen);
		}
		if( flags & 8 != 0 )
			fname = i.readUntil(0);
		if( flags & 16 != 0 )
			comments = i.readUntil(0);
		if( flags & 2 != 0 ) {
			var hcrc = i.readUInt16();
			// does not check header crc
		}
		return fname;
	}

	public static function readGZData( i : neko.io.Input, o : neko.io.Output, ?bufsize : Int ) : Int {
		if( bufsize == null ) bufsize = (1 << 16); // 65Ks
		var u = new Uncompress(-15);
		u.setFlushMode(Flush.SYNC);
		var buf = neko.Lib.makeString(bufsize);
		var out = neko.Lib.makeString(bufsize);
		var bufpos = bufsize;
		var tsize = 0;
		while( true ) {
			if( bufpos == buf.length ) {
				buf = refill(i,buf,0);
				bufpos = 0;
			}
			var r = u.run(buf,bufpos,out,0);
			if( r.read == 0 ) {
				if( bufpos == 0 )
					throw new neko.io.Eof();
				var len = buf.length - bufpos;
				neko.Lib.copyBytes(buf,0,buf,bufpos,len);
				buf = refill(i,buf,len);
				bufpos = 0;
			} else {
				bufpos += r.read;
				tsize += r.read;
				o.writeFullBytes(out,0,r.write);
				if( r.done )
					break;
			}
		}
		return tsize;
	}

	static function refill( i, buf : String, pos : Int ) {
		try {
			while( pos != buf.length ) {
				var k = i.readBytes(buf,pos,buf.length-pos);
				pos += k;
			}
		} catch( e : neko.io.Eof ) {
		}
		if( pos == 0 )
			throw new neko.io.Eof();
		if( pos != buf.length )
			buf = buf.substr(0,pos);
		return buf;
	}

	public static function readTarEntry( i : neko.io.Input ) {
		var fname = i.readUntil(0);
		if( fname.length == 0 ) {
			if( i.read(511+512) != neko.Lib.makeString(511+512) )
				throw "Invalid TAR end";
			return null;
		}
		i.read(99 - fname.length); // skip
		var fmod = parseOctal(i.readUntil(0));
		var uid = parseOctal(i.readUntil(0));
		var gid = parseOctal(i.readUntil(0));
		var fsize = parseOctal(i.readUntil(0));
		var mtime = i.readUntil(0);
		var crc = i.read(8);
		var type = i.readChar();
		var lname = i.readUntil(0);
		i.read(99 - lname.length); // skip
		var ustar = i.read(8);
		if( ustar != "ustar  \x00" && ustar != "ustar\x00\x00\x00" ) {
			trace(StringTools.urlEncode(ustar));
			throw "Not an tar ustar file";
		}
		var uname = i.readUntil(0);
		i.read(31 - uname.length);
		var gname = i.readUntil(0);
		i.read(31 - gname.length);
		var devmaj = parseOctal(i.readUntil(0));
		var devmin = parseOctal(i.readUntil(0));
		var prefix = i.readUntil(0);
		i.read(167 - prefix.length);
		return {
			fileName : fname,
			fileSize : fsize,
		};
	}

	public static function readTarData( i : neko.io.Input, o : neko.io.Output, size : Int, ?bufsize ) {
		if( bufsize == null ) bufsize = (1 << 16); // 65Ks
		var buf = neko.Lib.makeString(bufsize);
		var pad = Math.ceil(size / 512) * 512 - size;
		while( size > 0 ) {
			var n = i.readBytes(buf,0,if( size > bufsize ) bufsize else size);
			size -= n;
			o.writeFullBytes(buf,0,n);
		}
		i.read(pad);
	}

	static function parseOctal( n : String ) {
		var i = 0;
		for( p in 0...n.length ) {
			var c = n.charCodeAt(p);
			if( c < 48 || c > 55 )
				throw "Invalid octal char";
			i = (i * 8) + (c - 48);
		}
		return i;
	}

}