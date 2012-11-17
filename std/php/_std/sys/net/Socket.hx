/*
 * Copyright (C)2005-2012 Haxe Foundation
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
		untyped __php__("while (!feof($this->__s)) $b .= fgets($this->__s)");
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
