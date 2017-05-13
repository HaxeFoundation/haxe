/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.zip;
import haxe.zip.Entry;

// see http://www.pkware.com/documents/casestudies/APPNOTE.TXT

class Reader {

	var i : haxe.io.Input;

	public function new(i) {
		this.i = i;
	}

	function readZipDate() {
		var t = i.readUInt16();
		var hour = (t >> 11) & 31;
		var min = (t >> 5) & 63;
		var sec = t & 31;
		var d = i.readUInt16();
		var year = d >> 9;
		var month = (d >> 5) & 15;
		var day = d & 31;
		return new Date(year + 1980, month-1, day, hour, min, sec << 1);
	}

	function readExtraFields(length) {
		var fields = new List();
		while( length > 0 ) {
			if( length < 4 ) throw "Invalid extra fields data";
			var tag = i.readUInt16();
			var len = i.readUInt16();
			if( length < len ) throw "Invalid extra fields data";
			switch( tag ) {
			case 0x7075:
				var version = i.readByte();
				if( version != 1 ) {
					var data = new haxe.io.BytesBuffer();
					data.addByte(version);
					data.add(i.read(len-1));
					fields.add(FUnknown(tag,data.getBytes()));
				} else {
					var crc = i.readInt32();
					var name = i.read(len - 5).toString();
					fields.add(FInfoZipUnicodePath(name,crc));
				}
			default:
				fields.add(FUnknown(tag,i.read(len)));
			}
			length -= 4 + len;
		}
		return fields;
	}

	public function readEntryHeader() : Entry {
		var i = this.i;
		var h = i.readInt32();
		if( h == 0x02014B50 || h == 0x06054B50 )
			return null;
		if( h != 0x04034B50 )
			throw "Invalid Zip Data";
		var version = i.readUInt16();
		var flags = i.readUInt16();
		var utf8 = flags & 0x800 != 0;
		if( (flags & 0xF7F1) != 0 )
			throw "Unsupported flags "+flags;
		var compression = i.readUInt16();
		var compressed = (compression != 0);
		if( compressed && compression != 8 )
			throw "Unsupported compression "+compression;
		var mtime = readZipDate();
		var crc32 : Null<Int> = i.readInt32();
		var csize = i.readInt32();
		var usize = i.readInt32();
		var fnamelen = i.readInt16();
		var elen = i.readInt16();
		var fname = i.readString(fnamelen);
		var fields = readExtraFields(elen);
		if( utf8 )
			fields.push(FUtf8);
		var data = null;
		// we have a data descriptor that store the real crc/sizes
		// after the compressed data, let's wait for it
		if( (flags & 8) != 0 )
			crc32 = null;
		return {
			fileName : fname,
			fileSize : usize,
			fileTime : mtime,
			compressed : compressed,
			dataSize : csize,
			data : data,
			crc32 : crc32,
			extraFields : fields,
		};
	}

	public function read() : List<Entry> {
		var l = new List();
		var buf = null;
		var tmp = null;
		while( true ) {
			var e = readEntryHeader();
			if( e == null )
				break;
			// do we have a data descriptor? (see readEntryHeader)
			if( e.crc32 == null ) {
				if( e.compressed ) {
					#if neko
					// enter progressive mode : we use a different input which has
					// a temporary buffer, this is necessary since we have to uncompress
					// progressively, and after that we might have pending read data
					// that needs to be processed
					var bufSize = 65536;
					if( buf == null ) {
						buf = new haxe.io.BufferInput(i, haxe.io.Bytes.alloc(bufSize));
						tmp = haxe.io.Bytes.alloc(bufSize);
						i = buf;
					}
					var out = new haxe.io.BytesBuffer();
					var z = new neko.zip.Uncompress(-15);
					z.setFlushMode(neko.zip.Flush.SYNC);
					while( true ) {
						if( buf.available == 0 )
							buf.refill();
						var p = bufSize - buf.available;
						if( p != buf.pos ) {
							// because of lack of "srcLen" in zip api, we need to always be stuck to the buffer end
							buf.buf.blit(p, buf.buf, buf.pos, buf.available);
							buf.pos = p;
						}
						var r = z.execute(buf.buf, buf.pos, tmp, 0);
						out.addBytes(tmp, 0, r.write);
						buf.pos += r.read;
						buf.available -= r.read;
						if( r.done ) break;
					}
					e.data = out.getBytes();
					#else
					var bufSize = 65536;
					if( tmp == null )
						tmp = haxe.io.Bytes.alloc(bufSize);
					var out = new haxe.io.BytesBuffer();
					var z = new InflateImpl(i, false, false);
					while( true ) {
						var n = z.readBytes(tmp, 0, bufSize);
						out.addBytes(tmp, 0, n);
						if( n < bufSize )
							break;
					}
					e.data = out.getBytes();
					#end
				} else
					e.data = i.read(e.dataSize);
				e.crc32 = i.readInt32();
				if( e.crc32 == 0x08074b50 )
					e.crc32 = i.readInt32();
				e.dataSize = i.readInt32();
				e.fileSize = i.readInt32();
				// set data to uncompressed
				e.dataSize = e.fileSize;
				e.compressed = false;
			} else
				e.data = i.read(e.dataSize);
			l.add(e);
		}
		return l;
	}

	public static function readZip( i : haxe.io.Input ) {
		var r = new Reader(i);
		return r.read();
	}

	public static function unzip( f : Entry ) {
		if( !f.compressed )
			return f.data;
		var c = new haxe.zip.Uncompress(-15);
		var s = haxe.io.Bytes.alloc(f.fileSize);
		var r = c.execute(f.data,0,s,0);
		c.close();
		if( !r.done || r.read != f.data.length || r.write != f.fileSize )
			throw "Invalid compressed data for "+f.fileName;
		f.compressed = false;
		f.dataSize = f.fileSize;
		f.data = s;
		return f.data;
	}

}
