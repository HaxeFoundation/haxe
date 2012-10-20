/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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

#if !haxe3

class Writer {

	/*
	* The next constant is required for computing the Central
	* Directory Record(CDR) size. CDR consists of some fields
	* of constant size and a filename. Constant represents
	* total length of all fields with constant size for each
	* file in archive
	*/
	private static var CENTRAL_DIRECTORY_RECORD_FIELDS_SIZE = 46;

	/*
	* The following constant is the total size of all fields
	* of Local File Header. It's required for calculating
	* offset of start of central directory record
	*/
	private static var LOCAL_FILE_HEADER_FIELDS_SIZE = 30;

	static function writeZipDate( o : haxe.io.Output, date : Date ) {
		var hour = date.getHours();
		var min = date.getMinutes();
		var sec = date.getSeconds() >> 1;
		o.writeUInt16( (hour << 11) | (min << 5) | sec );
		var year = date.getFullYear() - 1980;
		var month = date.getMonth() + 1;
		var day = date.getDate();
		o.writeUInt16( (year << 9) | (month << 5) | day );
	}

	static function writeZipEntry( o : haxe.io.Output, level, f : { data : haxe.io.Bytes, fileName : String, fileTime : Date } ) {
		var fdata = f.data, cdata = null, crc32, compressed = true;
		o.writeUInt30(0x04034B50);
		o.writeUInt16(0x0014); // version
		o.writeUInt16(0); // flags
		if( fdata == null ) {
			fdata = haxe.io.Bytes.alloc(0);
			cdata = haxe.io.Bytes.ofString("XXXXXX");
			crc32 = haxe.Int32.ofInt(0);
			compressed = false;
		} else {
			crc32 = CRC32.encode(f.data);
			cdata = Compress.run( f.data, level );
		}
		o.writeUInt16(compressed?8:0);
		writeZipDate(o,f.fileTime);
		o.writeInt32(crc32);
		o.writeUInt30(cdata.length - 6);
		o.writeUInt30(fdata.length);
		o.writeUInt16(f.fileName.length);
		o.writeUInt16(0);
		o.writeString(f.fileName);
		if( cdata != null ) o.writeFullBytes(cdata,2,cdata.length-6);
		return {
			compressed : compressed,
			fileName : f.fileName,
			dlen : fdata.length,
			clen : cdata.length - 6,
			date : f.fileTime,
			crc32 : crc32,
		};
	}

	public static function writeZip( o : haxe.io.Output, files, compressionLevel : Int ) {
		var files = Lambda.map(files,callback(writeZipEntry,o,compressionLevel));
		var cdr_size = 0;
		var cdr_offset = 0;
		for( f in files ) {
			var namelen = f.fileName.length;
			o.writeUInt30(0x02014B50); // header
			o.writeUInt16(0x0014); // version made-by
			o.writeUInt16(0x0014); // version
			o.writeUInt16(0); // flags
			o.writeUInt16(f.compressed?8:0);
			writeZipDate(o,f.date);
			o.writeInt32(f.crc32);
			o.writeUInt30(f.clen);
			o.writeUInt30(f.dlen);
			o.writeUInt16(namelen);
			o.writeUInt16(0); //extra field length always 0
			o.writeUInt16(0); //comment length always 0
			o.writeUInt16(0); //disk number start
			o.writeUInt16(0); //internal file attributes
			o.writeUInt30(0);	//external file attributes
			o.writeUInt30(cdr_offset); //relative offset of local header
			o.writeString(f.fileName);
			cdr_size += CENTRAL_DIRECTORY_RECORD_FIELDS_SIZE + namelen;
			cdr_offset += LOCAL_FILE_HEADER_FIELDS_SIZE + namelen + f.clen;
		}
		//end of central dir signature
		o.writeUInt30(0x06054B50);
		//number of this disk
		o.writeUInt16(0);
		//number of the disk with the start of the central directory
		o.writeUInt16(0);
		//total number of entries in the central directory on this disk
		o.writeUInt16(files.length);
		//total number of entries in the central directory
		o.writeUInt16(files.length);
		//size of the central directory record
		o.writeUInt30(cdr_size);
		//offset of start of central directory with respect to the starting disk number
		o.writeUInt30(cdr_offset);
		// .ZIP file comment length
		o.writeUInt16(0);
	}


}

#end
