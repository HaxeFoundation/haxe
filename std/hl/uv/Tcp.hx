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

package hl.uv;

@:hlNative("uv")
class Tcp extends Stream {
	public function new(?loop:Loop) {
		if (loop == null)
			loop = Loop.getDefault();
		super(tcp_init_wrap(loop));
	}

	public function connect(host:sys.net.Host, port:Int, onConnected:Bool->Void) {
		var h = tcp_connect_wrap(handle, host.ip, port, onConnected);
		if (h == null)
			throw haxe.io.Error.Custom("Failed to connect to " + host + ":" + port);
	}

	public function bind(host:sys.net.Host, port:Int) {
		if (!tcp_bind_wrap(handle, host.ip, port))
			throw haxe.io.Error.Custom("Failed to bind socket to " + host + ":" + port);
	}

	public function accept() {
		var client = handle == null ? null : tcp_accept_wrap(handle);
		if (client == null)
			throw new haxe.io.Eof();
		return new Stream(client);
	}

	public function noDelay(b:Bool) {
		tcp_nodelay_wrap(handle, b);
	}

	static function tcp_init_wrap(loop:Loop):HandleData {
		return null;
	}

	static function tcp_connect_wrap(h:HandleData, host:Int, port:Int, onConnected:Bool->Void):HandleData {
		return null;
	}

	static function tcp_bind_wrap(h:HandleData, host:Int, port:Int):Bool {
		return false;
	}

	static function tcp_accept_wrap(h:HandleData):HandleData {
		return null;
	}

	static function tcp_nodelay_wrap(h:HandleData, b:Bool):Void {}
}
