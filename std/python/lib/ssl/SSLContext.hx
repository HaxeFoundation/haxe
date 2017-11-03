/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package python.lib.ssl;

import python.lib.ssl.SSLSocket;

@:pythonImport("ssl", "SSLContext")
extern class SSLContext {
	public function wrap_socket(s:python.lib.net.Socket, server_side:Bool = false, do_handshake_on_connect:Bool = true, suppress_ragged_eofs:Bool = true, server_hostname:String = null ):python.lib.ssl.SSLSocket;
	public var options:Int;

	//public function load_default_certs():Void;
	//public function load_cert_chain(certfile:String, keyfile:String = null, password:String = null):Void;
	//public function set_servername_callback(callback:SSLSocket -> String -> SSLContext -> Void ):Void;
	//public var check_hostname:Bool;
	//public var verify_mode:Int;
}