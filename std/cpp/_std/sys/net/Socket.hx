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

import haxe.io.Error;

private class SocketInput extends haxe.io.Input {

	var __s : Dynamic;

	public function new(s) {
		__s = s;
	}

	public override function readByte() {
		return try {
			socket_recv_char(__s);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else if( __s == null )
				throw Custom(e);
			else
				throw new haxe.io.Eof();
		}
	}

	public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		var r;
		if (__s==null)
			throw "Invalid handle";
		try {
			r = socket_recv(__s,buf.getData(),pos,len);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
		if( r == 0 )
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close(__s);
	}

	private static var socket_recv = cpp.Lib.load("std","socket_recv",4);
	private static var socket_recv_char = cpp.Lib.load("std","socket_recv_char",1);
	private static var socket_close = cpp.Lib.load("std","socket_close",1);

}

private class SocketOutput extends haxe.io.Output {

	var __s : Dynamic;

	public function new(s) {
		__s = s;
	}

	public override function writeByte( c : Int ) {
		if (__s==null)
			throw "Invalid handle";
		try {
			socket_send_char(__s, c);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
		return try {
			socket_send(__s, buf.getData(), pos, len);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw Blocked;
			else
				throw Custom(e);
		}
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close(__s);
	}

	private static var socket_close = cpp.Lib.load("std","socket_close",1);
	private static var socket_send_char = cpp.Lib.load("std","socket_send_char",2);
	private static var socket_send = cpp.Lib.load("std","socket_send",4);

}


@:coreApi
class Socket {

	private var __s : Dynamic;
	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;
	public var custom : Dynamic;

	public function new() : Void {
		__s = socket_new(false);
		input = new SocketInput(__s);
		output = new SocketOutput(__s);
	}

	public function close() : Void {
		socket_close(__s);
		untyped {
			var input : SocketInput = cast input;
			var output : SocketOutput = cast output;
			input.__s = null;
			output.__s = null;
		}
		input.close();
		output.close();
	}

	public function read() : String {
		var bytes:haxe.io.BytesData = socket_read(__s);
		if (bytes==null) return "";
		return bytes.toString();
	}

	public function write( content : String ) : Void {
		socket_write(__s, haxe.io.Bytes.ofString(content).getData() );
	}

	public function connect(host : Host, port : Int) : Void {
		try {
			socket_connect(__s, host.ip, port);
		} catch( s : String ) {
			if( s == "std@socket_connect" )
				throw "Failed to connect on "+(try host.reverse() catch( e : Dynamic ) host.toString())+":"+port;
			else
				cpp.Lib.rethrow(s);
		}
	}

	public function listen(connections : Int) : Void {
		socket_listen(__s, connections);
	}

	public function shutdown( read : Bool, write : Bool ) : Void {
		socket_shutdown(__s,read,write);
	}

	public function bind(host : Host, port : Int) : Void {
		socket_bind(__s, host.ip, port);
	}

	public function accept() : Socket {
		var c = socket_accept(__s);
		var s = Type.createEmptyInstance(Socket);
		s.__s = c;
		s.input = new SocketInput(c);
		s.output = new SocketOutput(c);
		return s;
	}

	public function peer() : { host : Host, port : Int } {
		var a : Dynamic = socket_peer(__s);
		var h = new Host("127.0.0.1");
		untyped h.ip = a[0];
		return { host : h, port : a[1] };
	}

	public function host() : { host : Host, port : Int } {
		var a : Dynamic = socket_host(__s);
		var h = new Host("127.0.0.1");
		untyped h.ip = a[0];
		return { host : h, port : a[1] };
	}

	public function setTimeout( timeout : Float ) : Void {
		socket_set_timeout(__s, timeout);
	}

	public function waitForRead() : Void {
		select([this],null,null,null);
	}

	public function setBlocking( b : Bool ) : Void {
		socket_set_blocking(__s,b);
	}

	public function setFastSend( b : Bool ) : Void {
		throw "Not implemented";
	}

	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float ) : {read: Array<Socket>,write: Array<Socket>,others: Array<Socket>} {
		var neko_array = socket_select(read,write,others, timeout);
		if (neko_array==null)
			throw "Select error";
		return {
			read: neko_array[0],
			write: neko_array[1],
			others: neko_array[2]
		};
	}

	private static var socket_new = cpp.Lib.load("std","socket_new",1);
	private static var socket_close = cpp.Lib.load("std","socket_close",1);
	private static var socket_write = cpp.Lib.load("std","socket_write",2);
	private static var socket_read = cpp.Lib.load("std","socket_read",1);
	private static var socket_connect = cpp.Lib.load("std","socket_connect",3);
	private static var socket_listen = cpp.Lib.load("std","socket_listen",2);
	private static var socket_select = cpp.Lib.load("std","socket_select",4);
	private static var socket_bind = cpp.Lib.load("std","socket_bind",3);
	private static var socket_accept = cpp.Lib.load("std","socket_accept",1);
	private static var socket_peer = cpp.Lib.load("std","socket_peer",1);
	private static var socket_host = cpp.Lib.load("std","socket_host",1);
	private static var socket_set_timeout = cpp.Lib.load("std","socket_set_timeout",2);
	private static var socket_shutdown = cpp.Lib.load("std","socket_shutdown",3);
	private static var socket_set_blocking = cpp.Lib.load("std","socket_set_blocking",2);

}
