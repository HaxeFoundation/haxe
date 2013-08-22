/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.io;

class BytesInput extends Input {
	var b : BytesData;
	#if !flash9
	var pos : Int;
	var len : Int;
	var totlen : Int;
	#end
	#if js
	var v : js.html.DataView;
	#end
	
	/** The current position in the stream in bytes. */
	public var position(get,set) : Int;

	/** The length of the stream in bytes. */
	public var length(get,never) : Int;

	public function new( b : Bytes, ?pos : Int, ?len : Int ) {
		if( pos == null ) pos = 0;
		if( len == null ) len = b.length - pos;
		if( pos < 0 || len < 0 || pos + len > b.length ) throw Error.OutsideBounds;
		#if flash9
		var ba = b.getData();
		ba.position = pos;
		if( len != ba.bytesAvailable ) {
			// truncate
			this.b = new flash.utils.ByteArray();
			ba.readBytes(this.b,0,len);
		} else
			this.b = ba;
		this.b.endian = flash.utils.Endian.LITTLE_ENDIAN;
		#elseif js
		this.b = b.getData();
		this.pos = pos;
		this.len = len;
		this.totlen = len;
		this.v = new js.html.DataView(this.b.buffer);
		#else
		this.b = b.getData();
		this.pos = pos;
		this.len = len;
		this.totlen = len;
		#end
	}
	
	inline function get_position() : Int {
		#if flash9
		return b.position;
		#else
		return pos;
		#end
	}

	inline function get_length() : Int {
		#if flash9
		return b.length;
		#else
		return totlen;
		#end
	}
	
	function set_position( p : Int ) : Int {
		#if flash9
		return b.position = p;
		#else
		return pos = p;
		#end
	}
	
	public override function readByte() : Int {
		#if flash9
			return try b.readUnsignedByte() catch( e : Dynamic ) throw new Eof();
		#else
			if( this.len == 0 )
				throw new Eof();
			len--;
			#if neko
			return untyped __dollar__sget(b,pos++);
			#elseif php
			return untyped __call__("ord", b[pos++]);
			#elseif cpp
			return untyped b[pos++];
			#elseif java
			return untyped b[pos++] & 0xFF;
			#elseif js
			return v.getUint8(pos++);
			#else
			return b[pos++];
			#end
		#end
	}

	public override function readBytes( buf : Bytes, pos, len ) : Int {
		#if !neko
			if( pos < 0 || len < 0 || pos + len > buf.length )
				throw Error.OutsideBounds;
		#end
		#if flash9
			var avail : Int = b.bytesAvailable;
			if( len > avail && avail > 0 ) len = avail;
			try b.readBytes(buf.getData(),pos,len) catch( e : Dynamic ) throw new Eof();
		#elseif java
			var avail : Int = this.len;
			if ( len > avail ) len = avail;
			if (len == 0)
				throw new Eof();
			java.lang.System.arraycopy(this.b, this.pos, buf.getData(), pos, len);
			this.pos += len;
			this.len -= len;
		#elseif cs
			var avail : Int = this.len;
			if ( len > avail ) len = avail;
			if (len == 0)
				throw new Eof();
			cs.system.Array.Copy(this.b,this.pos, buf.getData(), pos, len);
			this.pos += len;
			this.len -= len;
		#else
			if( this.len == 0 && len > 0 )
				throw new Eof();
			if( this.len < len )
				len = this.len;
			#if neko
			try untyped __dollar__sblit(buf.getData(),pos,b,this.pos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
			#elseif php
			untyped __php__("$buf->b = substr($buf->b, 0, $pos) . substr($this->b, $this->pos, $len) . substr($buf->b, $pos+$len)");
			#elseif js
			buf.getData().set(b.subarray(this.pos, this.pos + len), pos);
			#else
			var b1 = b;
			var b2 = buf.getData();
			for( i in 0...len )
				b2[pos+i] = b1[this.pos+i];
			#end
			this.pos += len;
			this.len -= len;
		#end
		return len;
	}
	#if js
	override function readInt8() {
		try return v.getInt8(pos++)
		catch(e:js.html.DOMError) throw new Eof();
	}
	override function readInt16() {
		try {
			var f:Int = v.getInt16(pos, !bigEndian);
			pos += 2;
			return f;
		} catch(e:js.html.DOMError) throw new Eof();
	}
	override function readUInt16() {
		try {
			var f:Int = v.getUint16(pos, !bigEndian);
			pos += 2;
			return f;
		} catch(e:js.html.DOMError) throw new Eof();
	}
	override function readInt32() {
		try {
			var f:Int = v.getInt32(pos, !bigEndian);
			pos += 4;
			return f;
		} catch(e:js.html.DOMError) throw new Eof();
	}
	override function readFloat() {
		try {
			var f:Float = v.getFloat32(pos, !bigEndian);
			pos += 4;
			return f;
		} catch(e:js.html.DOMError) throw new Eof();
	}
	override function readDouble() {
		try {
			var f:Float = v.getFloat64(pos, !bigEndian);
			pos += 8;
			return f;
		} catch(e:js.html.DOMError) throw new Eof();
	}
	#elseif flash9
	override function set_bigEndian(e) {
		bigEndian = e;
		b.endian = e ? flash.utils.Endian.BIG_ENDIAN : flash.utils.Endian.LITTLE_ENDIAN;
		return e;
	}

	override function readFloat() {
		return try b.readFloat() catch( e : Dynamic ) throw new Eof();
	}

	override function readDouble() {
		return try b.readDouble() catch( e : Dynamic ) throw new Eof();
	}

	override function readInt8() {
		return try b.readByte() catch( e : Dynamic ) throw new Eof();
	}

	override function readInt16() {
		return try b.readShort() catch( e : Dynamic ) throw new Eof();
	}

	override function readUInt16() : Int {
		return try b.readUnsignedShort() catch( e : Dynamic ) throw new Eof();
	}

	override function readInt32() : Int {
		return try b.readInt() catch( e : Dynamic ) throw new Eof();
	}

	override function readString( len : Int ) {
		return try b.readUTFBytes(len) catch( e : Dynamic ) throw new Eof();
	}

	#end

}
