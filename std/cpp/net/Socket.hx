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
package cpp.net;

typedef SocketHandle = Dynamic;

class Socket {

	private var __s : SocketHandle;
	public var input(default,null) : SocketInput;
	public var output(default,null) : SocketOutput;
	public var custom : Dynamic;

	public function new( ?s ) {
		__s = if( s == null ) socket_new(false) else s;
		input = new SocketInput(__s);
		output = new SocketOutput(__s);
	}

	public function close() : Void {
		socket_close(__s);
		untyped {
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

	public function write( content : String ) {
		socket_write(__s, haxe.io.Bytes.ofString(content).getData() );
	}

	public function connect(host : Host, port : Int) {
		try {
			socket_connect(__s, host.ip, port);
		} catch( s : String ) {
			if( s == "std@socket_connect" )
				throw "Failed to connect on "+(try host.reverse() catch( e : Dynamic ) host.toString())+":"+port;
			else
				cpp.Lib.rethrow(s);
		}
	}

	public function listen(connections : Int) {
		socket_listen(__s, connections);
	}

	public function shutdown( read : Bool, write : Bool ){
		socket_shutdown(__s,read,write);
	}

	public function bind(host : Host, port : Int) {
		socket_bind(__s, host.ip, port);
	}

	public function accept() : Socket {
		return new Socket(socket_accept(__s));
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

	public function setTimeout( timeout : Float ) {
		socket_set_timeout(__s, timeout);
	}

	public function waitForRead() {
		select([this],null,null,null);
	}

	public function setBlocking( b : Bool ) {
		socket_set_blocking(__s,b);
	}

	public static function newUdpSocket() {
		return new Socket(socket_new(true));
	}

	// STATICS
	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, timeout : Null<Float>) : {read: Array<Socket>,write: Array<Socket>,others: Array<Socket>} {
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
