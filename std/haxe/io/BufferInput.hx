/*
 * format - haXe File Formats
 *
 *  inflate format decompression algorithm
 *  Copyright (C) 2004-2008 Nicolas Cannasse
 *  Compliant with RFC 1950 and 1951
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
package haxe.io;

class BufferInput extends haxe.io.Input {

	public var i : haxe.io.Input;
	public var buf : haxe.io.Bytes;
	public var available : Int;
	public var pos : Int;

	public function new(i, buf, ?pos = 0, ?available = 0) {
		this.i = i;
		this.buf = buf;
		this.pos = pos;
		this.available = available;
	}

	public function refill() {
		if( pos > 0 ) {
			buf.blit(0, buf, pos, available);
			pos = 0;
		}
		available += i.readBytes(buf, available, buf.length - available);
	}

	override function readByte() {
		if( available == 0 ) refill();
		var c = buf.get(pos);
		pos++;
		available--;
		return c;
	}

	override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) {
		if( available == 0 ) refill();
		var size = if( len > available ) available else len;
		buf.blit(pos, this.buf, this.pos, size);
		this.pos += size;
		this.available -= size;
		return size;
	}

}