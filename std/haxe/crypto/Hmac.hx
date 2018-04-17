/*
 * Copyright (C)2005-2018 Haxe Foundation
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
package haxe.crypto;

/**
    Hash methods for Hmac calculation.
*/
enum HashMethod {
	MD5;
	SHA1;
	SHA256;
}

/**
    Calculates a Hmac of the given Bytes using a HashMethod.
*/
class Hmac {
	
	var method : HashMethod;
	var blockSize : Int;
	var length : Int;
	
	public function new( hashMethod : HashMethod ) {
		method = hashMethod;
		blockSize = switch ( hashMethod ) {
			case MD5, SHA1, SHA256: 64;
		}
		length = switch ( hashMethod ) {
			case MD5: 16;
			case SHA1: 20;
			case SHA256: 32;
		}
	}
	
	inline function doHash( b : haxe.io.Bytes ) : haxe.io.Bytes {
		return switch ( method ) {
			case MD5: Md5.make(b);
			case SHA1: Sha1.make(b);
			case SHA256: Sha256.make(b);
		}
	}
	
	function nullPad( s : haxe.io.Bytes, chunkLen : Int ) : haxe.io.Bytes {
		var r = chunkLen - (s.length % chunkLen);
		if(r == chunkLen && s.length != 0)
			return s;
		var sb = new haxe.io.BytesBuffer();
		sb.add(s);
		for(x in 0...r)
			sb.addByte(0);
		return sb.getBytes();
	}
	
	public function make( key : haxe.io.Bytes, msg : haxe.io.Bytes ) : haxe.io.Bytes {
		if(key.length > blockSize) {
			key = doHash(key);
		}
		key = nullPad(key, blockSize);

		var Ki = new haxe.io.BytesBuffer();
		var Ko = new haxe.io.BytesBuffer();
		for (i in 0...key.length) {
			Ko.addByte(key.get(i) ^ 0x5c);
			Ki.addByte(key.get(i) ^ 0x36);
		}
		// hash(Ko + hash(Ki + message))
		Ki.add(msg);
		Ko.add(doHash(Ki.getBytes()));
		return doHash(Ko.getBytes());
	}
	
}
