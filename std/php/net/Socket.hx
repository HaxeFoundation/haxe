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
 *
 * Contributor: Lee McColl Sylvester
 */
package php.net;

import sys.io.File;

typedef SocketHandle = FileHandle;

class Socket {
	private var __s : SocketHandle;
	public var input(default,null) : SocketInput;
	public var output(default,null) : SocketOutput;
	public var custom : Dynamic;

	public var protocol(default, null) : String;

	public function new( ?s ) {
		__s = s;
		input = untyped new SocketInput(__s);
		output = untyped new SocketOutput(__s);
		protocol = "tcp";
	}

	private function assignHandler() {
		untyped input.__f = __s;
		untyped output.__f = __s;
	}

	public function close() : Void {
		untyped __call__('fclose', __s);
		untyped {
			input.__f = null;
			output.__f = null;
		}
		input.close();
		output.close();
	}

	public function read() : String {
		var b = '';
		untyped __php__('while (!feof($this->__s)) $b .= fgets($this->__s)');
		return b;
	}

	public function write( content : String ) {
		return untyped __call__('fwrite', __s, content);
	}

	public function connect(host : Host, port : Int) {
		var errs = null;
		var errn = null;
		var r = untyped __call__('stream_socket_client', protocol + '://' +host._ip + ':' + port, errn, errs);
		Socket.checkError(r, errn, errs);
		__s = cast r;
		assignHandler();
	}

	public function listen(connections : Int) {
/* TODO: ??????
		var r = untyped __call__('socket_listen', __s, connections);
		checkError(r);
*/
	}

	public function shutdown( read : Bool, write : Bool ){
		var r;
		if(untyped __call__("function_exists", "stream_socket_shutdown")) {
			var rw = read && write ? 2 : (write ? 1 : (read ? 0 : 2));
			r = untyped __call__('stream_socket_shutdown', __s, rw);
		} else {
			r = untyped __call__('fclose', __s);
		}
		checkError(r, 0, 'Unable to Shutdown');
	}

	public function bind(host : Host, port : Int) {
		var errs = null;
		var errn = null;
		var r = untyped __call__('stream_socket_server', protocol + '://' +host._ip + ':' + port, errn, errs, (protocol=="udp") ? __php__('STREAM_SERVER_BIND') : __php__('STREAM_SERVER_BIND | STREAM_SERVER_LISTEN'));
		Socket.checkError(r, errn, errs);
		__s = cast r;
		assignHandler();
	}

	public function accept() : Socket {
		var r = untyped __call__('stream_socket_accept', __s);
		checkError(r, 0, 'Unable to accept connections on socket');
		return untyped new Socket(cast r);
	}

	private function hpOfString(s : String) {
		var parts = s.split(':');
		if(parts.length == 2) {
			return { host : new Host(parts[0]), port : Std.parseInt(parts[1]) };
		} else {
			return { host : new Host(parts[1].substr(2)), port : Std.parseInt(parts[2]) };
		}
	}

	public function peer() : { host : Host, port : Int } {
		var r : String = untyped __call__('stream_socket_get_name', __s, true);
		checkError(cast r, 0, 'Unable to retrieve the peer name');
		return hpOfString(r);
	}

	public function host() : { host : Host, port : Int } {
		var r : String = untyped __call__('stream_socket_get_name', __s, false);
		checkError(cast r, 0, 'Unable to retrieve the host name');
		return hpOfString(r);
	}

	public function setTimeout( timeout : Float ) {
		var s = Std.int(timeout);
		var ms = Std.int((timeout-s)*1000000);
		var r = untyped __call__('stream_set_timeout', __s, s, ms);
		checkError(r, 0, 'Unable to set timeout');
	}

	public function setBlocking( b : Bool ) {
		var r = untyped __call__('stream_set_blocking', __s, b);
		checkError(r, 0, 'Unable to block');
	}

	// STATICS
	public static function newUdpSocket() {
		var s = new Socket();
		s.protocol = "udp";
		return s;
	}

	public static function newSslSocket() {
		var s = new Socket();
		s.protocol = "ssl";
 		return s;
 	}

	private static function checkError(r : Bool, code : Int, msg : String) {
		if(!untyped __physeq__(r, false)) return;
		throw haxe.io.Error.Custom('Error ['+code+']: ' +msg);
	}

	private static function getType(isUdp : Bool) : Int {
		return isUdp ? untyped __php__('SOCK_DGRAM') : untyped __php__('SOCK_STREAM');
	}

	private static function getProtocol(protocol : String) : Int {
		return untyped __call__('getprotobyname', protocol);
 	}
}

enum SocketDomain {
	AfInet;
	AfInet6;
	AfUnix;
}
