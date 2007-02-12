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
package neko.net;

enum SocketHandle {
}

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
		return socket_read(__s);
	}

	public function write( content : String ) {
		socket_write(__s, untyped content.__s);
	}

	public function connect(host : Host, port : Int) {
		try {
			socket_connect(__s, host.__h, port);
		} catch( s : String ) {
			if( s == "std@socket_connect" )
				throw "Failed to connect on "+(try host.reverse() catch( e : Dynamic ) host.toString())+":"+port;
			else
				neko.Lib.rethrow(s);
		}
	}

	public function listen(connections : Int) {
		socket_listen(__s, connections);
	}

	public function shutdown( read : Bool, write : Bool ){
		socket_shutdown(__s,read,write);
	}

	public function bind(host : Host, port : Int) {
		socket_bind(__s, host.__h, port);
	}

	public function accept() : Socket {
		return new Socket(socket_accept(__s));
	}

	public function peer() : { host : Host, port : Int } {
		var a : Dynamic = socket_peer(__s);
		return { host : a[0], port : a[1] };
	}

	public function host() : { host : Host, port : Int } {
		var a : Dynamic = socket_host(__s);
		return { host : a[0], port : a[1] };
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
	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, timeout : Float) : {read: Array<Socket>,write: Array<Socket>,others: Array<Socket>} {
		var c = untyped __dollar__hnew( 1 );
		var f = function( a : Array<Socket> ){
			if( a == null ) return null;
			untyped {
				var r = __dollar__amake(a.length);
				var i = 0;
				while( i < a.length ){
					r[i] = a[i].__s;
					__dollar__hadd(c,a[i].__s,a[i]);
					i += 1;
				}
				return r;
			}
		}
		var neko_array = socket_select(f(read),f(write),f(others), timeout);

		var g = function( a ) : Array<Socket> {
			if( a == null ) return null;

			var r = new Array();
			var i = 0;
			while( i < untyped __dollar__asize(a) ){
				var t = untyped __dollar__hget(c,a[i],null);
				if( t == null ) throw "Socket object not found.";
				r[i] = t;
				i += 1;
			}
			return r;
		}

		return {
			read: g(neko_array[0]),
			write: g(neko_array[1]),
			others: g(neko_array[2])
		};
	}

	static function __init__() {
		neko.Lib.load("std","socket_init",0)();
	}

	private static var socket_new = neko.Lib.load("std","socket_new",1);
	private static var socket_close = neko.Lib.load("std","socket_close",1);
	private static var socket_write = neko.Lib.load("std","socket_write",2);
	private static var socket_read = neko.Lib.load("std","socket_read",1);
	private static var socket_connect = neko.Lib.load("std","socket_connect",3);
	private static var socket_listen = neko.Lib.load("std","socket_listen",2);
	private static var socket_select = neko.Lib.load("std","socket_select",4);
	private static var socket_bind = neko.Lib.load("std","socket_bind",3);
	private static var socket_accept = neko.Lib.load("std","socket_accept",1);
	private static var socket_peer = neko.Lib.load("std","socket_peer",1);
	private static var socket_host = neko.Lib.load("std","socket_host",1);
	private static var socket_set_timeout = neko.Lib.load("std","socket_set_timeout",2);
	private static var socket_shutdown = neko.Lib.load("std","socket_shutdown",3);
	private static var socket_set_blocking = neko.Lib.load("std","socket_set_blocking",2);

}
