/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package python.net;

import python.lib.Ssl;
import python.lib.ssl.Purpose;
import python.lib.socket.Socket as PSocket;
import python.lib.Socket in PSocketModule;
import sys.net.Host;

class SslSocket extends sys.net.Socket {
	var _timeout:Null<Float> = null;
	var _blocking:Null<Bool> = null;
	var _fastSend:Null<Bool> = null;

	function wrapSocketWithSslContext(hostName:String):Void {
		#if (python_version >= 3.4)
		var context = Ssl.create_default_context(Purpose.SERVER_AUTH);
		#else
		// hopefully these options are good enough
		var context = new python.lib.ssl.SSLContext(Ssl.PROTOCOL_SSLv23);
		context.verify_mode = Ssl.CERT_REQUIRED;
		context.set_default_verify_paths();
		context.options |= Ssl.OP_NO_SSLv2;
		context.options |= Ssl.OP_NO_SSLv3;
		context.options |= Ssl.OP_NO_COMPRESSION;
		#end
		context.options |= Ssl.OP_NO_TLSv1 #if (python_version >= 3.4) | Ssl.OP_NO_TLSv1_1 #end; // python 3.4 | Ssl.OP_NO_TLSv1_1;
		__s = context.wrap_socket(__s, false, true, true, hostName);
		if (_timeout != null) {
			super.setTimeout(_timeout);
		}

		if (_blocking != null) {
			super.setBlocking(_blocking);
		}

		if (_fastSend != null) {
			super.setFastSend(_fastSend);
		}
		__rebuildIoStreams();
	}

	public override function connect(host:Host, port:Int):Void {
		wrapSocketWithSslContext(host.host);
		super.connect(host, port);
	}

	public override function bind(host:Host, port:Int):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public override function setTimeout(timeout:Float):Void {
		_timeout = timeout;
		super.setTimeout(_timeout);
	}

	public override function setBlocking(b:Bool):Void {
		_blocking = b;
		super.setBlocking(_blocking);
	}

	public override function setFastSend(b:Bool):Void {
		_fastSend = b;
		super.setFastSend(_fastSend);
	}
}
