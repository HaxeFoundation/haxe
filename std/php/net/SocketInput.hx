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
package php.net;

typedef SocketInput = php.io.FileInput;

/*
import php.net.Socket;
import haxe.io.Error;
class SocketInput extends haxe.io.Input {

	var __s : SocketHandle;

	public function new(s) {
		__s = s;
	}

	public override function readByte() {
		var r = untyped __call__('socket_read', __s, 1);
		untyped Socket.checkError(r);
		if(r == 0) return throw new haxe.io.Eof();
		return untyped __call__('ord', r);
	}

	public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		var r : String = untyped __call__('socket_read', __s, len);
		untyped Socket.checkError(r);
		var b = haxe.io.Bytes.ofString(r);
		if(r.length == 0) return throw new haxe.io.Eof();
		s.blit(pos, b, 0, r.length);
		return r.length;
	}

	public override function close() {
		super.close();
		if( __s != null ) untyped __call__('socket_close', __s);
	}
}

*/