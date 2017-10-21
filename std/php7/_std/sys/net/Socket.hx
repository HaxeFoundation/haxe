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
package sys.net;

import php.*;
import php.Global.*;
import php.Const.*;
import sys.io.FileInput;
import sys.io.FileOutput;

@:coreApi
class Socket {

	private var __s : Resource;
	private var protocol : String;
	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;
	public var custom : Dynamic;

	public function new() : Void {
		input = @:privateAccess new sys.io.FileInput(null);
		output = @:privateAccess new sys.io.FileOutput(null);
		protocol = "tcp";
	}

	private function assignHandler() : Void {
		@:privateAccess (cast input:FileInput).__f = __s;
		@:privateAccess (cast output:FileOutput).__f = __s;
	}

	public function close() : Void {
		fclose(__s);
		@:privateAccess (cast input:FileInput).__f = null;
		@:privateAccess (cast output:FileOutput).__f = null;
		input.close();
		output.close();
	}

	public function read() : String {
		var b = '';
		while(!feof(__s)) Syntax.binop(b, '.=', fgets(__s));
		return b;
	}

	public function write( content : String ) : Void {
		fwrite(__s, content);
	}

	public function connect(host : Host, port : Int) : Void {
		var errs = null;
		var errn = null;
		var r = stream_socket_client(protocol + '://' + host.host + ':' + port, errn, errs);
		Socket.checkError(r, errn, errs);
		__s = r;
		assignHandler();
	}

	public function listen(connections : Int) : Void {
		throw "Not implemented";
/* TODO: ??????
		var r = socket_listen(__s, connections);
		checkError(r);
*/
	}

	public function shutdown( read : Bool, write : Bool ) : Void {
		var rw = read && write ? 2 : (write ? 1 : (read ? 0 : 2));
		var r = stream_socket_shutdown(__s, rw);
		checkError(r, 0, 'Unable to Shutdown');
	}

	public function bind(host : Host, port : Int) : Void {
		var errs = Boot.deref(null);
		var errn = Boot.deref(null);
		var r = stream_socket_server(
			protocol + '://' + host.host + ':' + port,
			errn,
			errs,
			(protocol == "udp" ? STREAM_SERVER_BIND : STREAM_SERVER_BIND | STREAM_SERVER_LISTEN)
		);
		Socket.checkError(r, errn, errs);
		__s = cast r;
		assignHandler();
	}

	public function accept() : Socket {
		var r = stream_socket_accept(__s);
		checkError(r, 0, 'Unable to accept connections on socket');
		var s = new Socket();
		s.__s = r;
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
		var r = stream_socket_get_name(__s, true);
		checkError(r, 0, 'Unable to retrieve the peer name');
		return hpOfString(r);
	}

	public function host() : { host : Host, port : Int } {
		var r = stream_socket_get_name(__s, false);
		checkError(r, 0, 'Unable to retrieve the host name');
		return hpOfString(r);
	}

	public function setTimeout( timeout : Float ) : Void {
		var s = Std.int(timeout);
		var ms = Std.int((timeout - s) * 1000000);
		var r = stream_set_timeout(__s, s, ms);
		checkError(r, 0, 'Unable to set timeout');
	}

	public function setBlocking( b : Bool ) : Void {
		var r = stream_set_blocking(__s, b);
		checkError(r, 0, 'Unable to block');
	}

	public function setFastSend( b : Bool ) : Void {
		throw "Not implemented";
	}

	public function waitForRead() : Void {
		select([this], null, null);
	}

	private static function checkError(r : Bool, code : Int, msg : String) : Void {
		if(r != false) return;
		throw haxe.io.Error.Custom('Error [$code]: $msg');
	}

	private static function getType(isUdp : Bool) : Int {
		return isUdp ? SOCK_DGRAM : SOCK_STREAM;
	}

	private static function getProtocol(protocol : String) : Int {
		return getprotobyname(protocol);
 	}

	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> }
	{
		throw "Not implemented";
		return null;
	}

}
