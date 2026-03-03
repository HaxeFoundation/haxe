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

package sys.ssl;

#if lua_vanilla

class Socket extends sys.net.Socket {
	static inline function notImplemented():Dynamic
		throw new haxe.exceptions.NotImplementedException("sys.ssl.Socket cannot be used with -D lua-vanilla because it requires luasec");

	public function handshake():Void notImplemented();
	override public function connect(host:sys.net.Host, port:Int):Void notImplemented();
	override public function close():Void notImplemented();
}

#else

import sys.net.Host;
import sys.net.SocketInput;
import sys.net.SocketOutput;

import lua.lib.luasocket.Socket as LuaSocket;
import lua.lib.luasec.Ssl as LuaSecSsl;
import lua.lib.luasec.SslTcpClient;

import haxe.io.Bytes;
import haxe.io.Error;

@:coreApi(check = Off)
class Socket extends sys.net.Socket {
    var _sslSocket:SslTcpClient;

    private function wrap(sock:LuaSocket):SslTcpClient {
        var res = LuaSecSsl.wrap(sock, {mode: Client, protocol: Any});
        if (res.message != null) {
			throw 'Socket Error : ${res.message}';
        }
        return res.result;
    }

    public function handshake():Void {
        var res = this._sslSocket.dohandshake();
        if (res.message != null) {
			throw 'Handshake Error : ${res.message}';
        }
    }

    public override function connect(host:Host, port:Int):Void {
        var res = LuaSocket.connect(host.host, port);
		if (res.message != null)
			throw 'Socket Error : ${res.message}';
        var sslSock = this.wrap(res.result);
		input = new SocketInput(sslSock);
		output = new SocketOutput(sslSock);
		_sslSocket = sslSock;
		_sslSocket.settimeout(timeout);
        this.handshake();
    }

    public override function close():Void {
		_sslSocket.close();
    }
}

#end
