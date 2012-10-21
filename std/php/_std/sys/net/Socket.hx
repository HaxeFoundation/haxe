/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
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
 */
package sys.net;

import sys.io.File;

@:coreApi
class Socket {

	private var __s : FileHandle;
	private var protocol : String;
	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;
	public var custom : Dynamic;

	public function new() : Void {
		input = untyped new sys.io.FileInput(null);
		output = untyped new sys.io.FileOutput(null);
		protocol = "tcp";
	}

	private function assignHandler() : Void {
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

	public function write( content : String ) : Void {
		return untyped __call__('fwrite', __s, content);
	}

	public function connect(host : Host, port : Int) : Void {
		var errs = null;
		var errn = null;
		var r = untyped __call__('stream_socket_client', protocol + '://' +host._ip + ':' + port, errn, errs);
		Socket.checkError(r, errn, errs);
		__s = cast r;
		assignHandler();
	}

	public function listen(connections : Int) : Void {
		throw "Not implemented";
/* TODO: ??????
		var r = untyped __call__('socket_listen', __s, connections);
		checkError(r);
*/
	}

	public function shutdown( read : Bool, write : Bool ) : Void {
		var r;
		if(untyped __call__("function_exists", "stream_socket_shutdown")) {
			var rw = read && write ? 2 : (write ? 1 : (read ? 0 : 2));
			r = untyped __call__('stream_socket_shutdown', __s, rw);
		} else {
			r = untyped __call__('fclose', __s);
		}
		checkError(r, 0, 'Unable to Shutdown');
	}

	public function bind(host : Host, port : Int) : Void {
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
		var s = new Socket();
		s.__s = cast r;
		s.assignHandler();
		return s;
	}

	private function hpOfString(s : String) : { host : Host, port : Int } {
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

	public function setTimeout( timeout : Float ) : Void {
		var s = Std.int(timeout);
		var ms = Std.int((timeout-s)*1000000);
		var r = untyped __call__('stream_set_timeout', __s, s, ms);
		checkError(r, 0, 'Unable to set timeout');
	}

	public function setBlocking( b : Bool ) : Void {
		var r = untyped __call__('stream_set_blocking', __s, b);
		checkError(r, 0, 'Unable to block');
	}

	public function setFastSend( b : Bool ) : Void {
		throw "Not implemented";
	}

	public function waitForRead() : Void {
		select([this],null,null);
	}

	private static function checkError(r : Bool, code : Int, msg : String) : Void {
		if(!untyped __physeq__(r, false)) return;
		throw haxe.io.Error.Custom('Error ['+code+']: ' +msg);
	}

	private static function getType(isUdp : Bool) : Int {
		return isUdp ? untyped __php__('SOCK_DGRAM') : untyped __php__('SOCK_STREAM');
	}

	private static function getProtocol(protocol : String) : Int {
		return untyped __call__('getprotobyname', protocol);
 	}

	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> }
	{
		throw "Not implemented";
		return null;
	}

}
