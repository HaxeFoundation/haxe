/*
 * format - haXe File Formats
 *
 * Copyright (c) 2008, The haXe Project Contributors
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
package haxe.zip;

class Writer {

	/*
	* The next constant is required for computing the Central
	* Directory Record(CDR) size. CDR consists of some fields
	* of constant size and a filename. Constant represents
	* total length of all fields with constant size for each
	* file in archive
	*/
	inline static var CENTRAL_DIRECTORY_RECORD_FIELDS_SIZE = 46;

	/*
	* The following constant is the total size of all fields
	* of Local File Header. It's required for calculating
	* offset of start of central directory record
	*/
	inline static var LOCAL_FILE_HEADER_FIELDS_SIZE = 30;

	var o : haxe.io.Output;
	var files : List<{ name : String, compressed : Bool, clen : Int, size : Int, crc : Int, date : Date, fields : haxe.io.Bytes }>;

	public function new( o : haxe.io.Output ) {
		this.o = o;
		files = new List();
	}

	function writeZipDate( date : Date ) {
		var hour = date.getHours();
		var min = date.getMinutes();
		var sec = date.getSeconds() >> 1;
		o.writeUInt16( (hour << 11) | (min << 5) | sec );
		var year = date.getFullYear() - 1980;
		var month = date.getMonth() + 1;
		var day = date.getDate();
		o.writeUInt16( (year << 9) | (month << 5) | day );
	}

	public function writeEntryHeader( f : Entry ) {
		var o = this.o;
		var flags = 0;
		for( e in f.extraFields )
			switch( e ) {
			case FUtf8: flags |= 0x800;
			default:
			}
		o.writeInt32(0x04034B50);
		o.writeUInt16(0x0014); // version
		o.writeUInt16(flags); // flags
		if( f.data == null ) {
			f.fileSize = 0;
			f.dataSize = 0;
			f.crc32 = 0;
			f.compressed = false;
			f.data = haxe.io.Bytes.alloc(0);
		} else {
			if( f.crc32 == null ) {
				if( f.compressed ) throw "CRC32 must be processed before compression";
				f.crc32 = haxe.crypto.Crc32.make(f.data);
			}
			if( !f.compressed )
				f.fileSize = f.data.length;
			f.dataSize = f.data.length;
		}
		o.writeUInt16(f.compressed?8:0);
		writeZipDate(f.fileTime);
		o.writeInt32(f.crc32);
		o.writeInt32(f.dataSize);
		o.writeInt32(f.fileSize);
		o.writeUInt16(f.fileName.length);
		var e = new haxe.io.BytesOutput();
		for( f in f.extraFields )
			switch( f ) {
			case FInfoZipUnicodePath(name,crc):
				var namebytes = haxe.io.Bytes.ofString(name);
				e.writeUInt16(0x7075);
				e.writeUInt16(namebytes.length + 5);
				e.writeByte(1); // version
				e.writeInt32(crc);
				e.write(namebytes);
			case FUnknown(tag,bytes):
				e.writeUInt16(tag);
				e.writeUInt16(bytes.length);
				e.write(bytes);
			case FUtf8:
				// nothing
			}
		var ebytes = e.getBytes();
		o.writeUInt16(ebytes.length);
		o.writeString(f.fileName);
		o.write(ebytes);
		files.add({ name : f.fileName, compressed : f.compressed, clen : f.data.length, size : f.fileSize, crc : f.crc32, date : f.fileTime, fields : ebytes });
	}

	public function write( files : List<Entry> ) {
		for( f in files ) {
			writeEntryHeader(f);
			o.writeFullBytes(f.data,0,f.data.length);
		}
		writeCDR();
	}

	public function writeCDR() {
		var cdr_size = 0;
		var cdr_offset = 0;
		for ( f in files ) {
			var namelen = f.name.length;
			var extraFieldsLength = f.fields.length;
			o.writeInt32(0x02014B50); // header
			o.writeUInt16(0x0014); // version made-by
			o.writeUInt16(0x0014); // version
			o.writeUInt16(0); // flags
			o.writeUInt16(f.compressed?8:0);
			writeZipDate(f.date);
			o.writeInt32(f.crc);
			o.writeInt32(f.clen);
			o.writeInt32(f.size);
			o.writeUInt16(namelen);
			o.writeUInt16(extraFieldsLength);
			o.writeUInt16(0); //comment length always 0
			o.writeUInt16(0); //disk number start
			o.writeUInt16(0); //internal file attributes
			o.writeInt32(0); //external file attributes
			o.writeInt32(cdr_offset); //relative offset of local header
			o.writeString(f.name);
			o.write(f.fields);
			cdr_size += CENTRAL_DIRECTORY_RECORD_FIELDS_SIZE + namelen + extraFieldsLength;
			cdr_offset += LOCAL_FILE_HEADER_FIELDS_SIZE + namelen + extraFieldsLength + f.clen;
		}
		//end of central dir signature
		o.writeInt32(0x06054B50);
		//number of this disk
		o.writeUInt16(0);
		//number of the disk with the start of the central directory
		o.writeUInt16(0);
		//total number of entries in the central directory on this disk
		o.writeUInt16(files.length);
		//total number of entries in the central directory
		o.writeUInt16(files.length);
		//size of the central directory record
		o.writeInt32(cdr_size);
		//offset of start of central directory with respect to the starting disk number
		o.writeInt32(cdr_offset);
		// .ZIP file comment length
		o.writeUInt16(0);
	}

}
