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

package python.lib.ssl;

import python.lib.ssl.SSLSocket;

@:pythonImport("ssl", "SSLContext")
extern class SSLContext {
	function new(protocol:String):Void;
	#if (python_version >= 3.6)
	function wrap_socket(s:python.lib.socket.Socket, server_side:Bool = false, do_handshake_on_connect:Bool = true, suppress_ragged_eofs:Bool = true,
		server_hostname:String = null, session:SSLSession = null):python.lib.ssl.SSLSocket;
	#else
	function wrap_socket(s:python.lib.socket.Socket, server_side:Bool = false, do_handshake_on_connect:Bool = true, suppress_ragged_eofs:Bool = true,
		server_hostname:String = null):python.lib.ssl.SSLSocket;
	#end
	var options:Int;

	@:require(python_version >= 3.4)
	var check_hostname:Bool;

	var verify_mode:Int;
	function load_verify_locations(cafile:String = null, capath:String = null, cadata:String = null):Void;
	function set_default_verify_paths():Void;

	@:require(python_version >= 3.4)
	function load_default_certs():Void;
	// function load_cert_chain(certfile:String, keyfile:String = null, password:String = null):Void;
	// function set_servername_callback(callback:SSLSocket -> String -> SSLContext -> Void ):Void;
}
