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
package haxe.io;

class BytesInput extends Input {
	var b : BytesData;
	#if !flash9
	var pos : Int;
	var len : Int;
	#end

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
		#else
		this.b = b.getData();
		this.pos = pos;
		this.len = len;
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
			return b[pos++] & 0xFF;
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

	#if flash9
	override function setEndian(e) {
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

	#if haxe3

	override function readInt32() : Int {
		return try b.readInt() catch( e : Dynamic ) throw new Eof();
	}

	#else

	override function readInt31() {
		var n;
		try n = b.readInt() catch( e : Dynamic ) throw new Eof();
		if( (n >>> 30) & 1 != (n >>> 31) ) throw Error.Overflow;
		return n;
	}

	override function readUInt30() {
		var n;
		try n = b.readInt() catch( e : Dynamic ) throw new Eof();
		if( (n >>> 30) != 0 ) throw Error.Overflow;
		return n;
	}


	override function readInt32() : haxe.Int32 {
		return try cast b.readInt() catch( e : Dynamic ) throw new Eof();
	}

	#end

	override function readString( len : Int ) {
		return try b.readUTFBytes(len) catch( e : Dynamic ) throw new Eof();
	}

	#end

}
