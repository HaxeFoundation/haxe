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

class BytesBuffer {

	#if neko
	var b : Void; // neko string buffer
	#elseif flash9
	var b : flash.utils.ByteArray;
	#elseif php
	var b : String;
	#elseif cpp
	var b : BytesData;
	#elseif cs
	var b : cs.system.io.MemoryStream;
	#elseif java
	var b : java.io.ByteArrayOutputStream;
	#else
	var b : Array<Int>;
	#end

	public function new() {
		#if neko
		b = untyped StringBuf.__make();
		#elseif flash9
		b = new flash.utils.ByteArray();
		#elseif php
		b = "";
		#elseif cpp
		b = new BytesData();
		#elseif cs
		b = new cs.system.io.MemoryStream();
		#elseif java
		b = new java.io.ByteArrayOutputStream();
		#else
		b = new Array();
		#end
	}

	public inline function addByte( byte : Int ) {
		#if neko
		untyped StringBuf.__add_char(b,byte);
		#elseif flash9
		b.writeByte(byte);
		#elseif php
		b += untyped __call__("chr", byte);
		#elseif cpp
		b.push(untyped byte);
		#elseif cs
		b.WriteByte(byte);
		#elseif java
		b.write(byte);
		#else
		b.push(byte);
		#end
	}

	public inline function add( src : Bytes ) {
		#if neko
		untyped StringBuf.__add(b,src.getData());
		#elseif flash9
		b.writeBytes(src.getData());
		#elseif php
		b += cast src.getData();
		#elseif cs
		b.Write(src.getData(), 0, src.length);
		#elseif java
		b.write(src.getData(), 0, src.length);
		#else
		var b1 = b;
		var b2 = src.getData();
		for( i in 0...src.length )
			b.push(b2[i]);
		#end
	}

	public inline function addBytes( src : Bytes, pos : Int, len : Int ) {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > src.length ) throw Error.OutsideBounds;
		#end
		#if neko
		try untyped StringBuf.__add_sub(b,src.getData(),pos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash9
		b.writeBytes(src.getData(),pos,len);
		#elseif php
		b += untyped __call__("substr", src.b, pos, len);
		#elseif cs
		b.Write(src.getData(), pos, len);
		#elseif java
		b.write(src.getData(), pos, len);
		#else
		var b1 = b;
		var b2 = src.getData();
		for( i in pos...pos+len )
			b.push(b2[i]);
		#end
	}

	/**
		Returns either a copy or a reference of the current bytes.
		Once called, the buffer can no longer be used.
	**/
	public function getBytes() : Bytes untyped {
		#if neko
		var str = StringBuf.__string(b);
		var bytes = new Bytes(__dollar__ssize(str),str);
		#elseif flash9
		var bytes = new Bytes(b.length,b);
		b.position = 0;
		#elseif php
		var bytes = new Bytes(b.length, cast b);
		#elseif cs
		var buf = b.GetBuffer();
		var bytes = new Bytes(cast b.Length, buf);
		#elseif java
		var buf = b.toByteArray();
		var bytes = new Bytes(buf.length, buf);
		#else
		var bytes = new Bytes(b.length,b);
		#end
		b = null;
		return bytes;
	}

}
