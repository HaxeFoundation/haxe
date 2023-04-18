/*
 * Copyright (C)2005-2022 Haxe Foundation
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
 
package sys.net;

import lua.lib.luasocket.socket.TcpClient;
import lua.*;

import haxe.io.Bytes;
import haxe.io.Error;

class SocketInput extends haxe.io.Input {
	var tcp:TcpClient;

	public function new(tcp:TcpClient) {
		this.tcp = tcp;
	}

	override public function readByte():Int {
		var res = tcp.receive(1);
		if (res.message == "closed"){
			throw new haxe.io.Eof();
		}
		else if (res.message != null)
			throw 'Error : ${res.message}';
		return res.result.charCodeAt(0);
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var leftToRead = len;
		var b = s.getData();
		if (pos < 0 || len < 0 || pos + len > s.length)
			throw haxe.io.Error.OutsideBounds;
		var readCount = 0;
		try {
			while (leftToRead > 0) {
				b[pos] = cast readByte();
				pos++;
				readCount++;
				leftToRead--;
			}
		} catch (e:haxe.io.Eof) {
			if (readCount == 0) {
				throw e;
			}
		}
		return readCount;
	}

}