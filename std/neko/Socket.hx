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
package neko;

enum Host {
}

class Socket {

	private var __s : Void;

	public function new() {
		__s = socket_new(false);
	}

	public function close() : Void {
		socket_close(__s);
	}

	public function sendChar(chr : Int) : Void {
		socket_send_char(__s, chr);
	}

	public function send(buffer : String, pos : Int, len : Int) : Int {
		return socket_send(__s, untyped buffer.__s, pos, len);
	}

	public function receive(buffer : String, pos : Int, len : Int) : Int {
		return socket_recv(__s, untyped buffer.__s, pos, len);
	}

	public function receiveChar() : Int {
		return socket_recv_char(__s);
	}

	public function write(content : String) {
		socket_write(__s, untyped content.__s);
	}

	public function read() : String	{
		return new String(socket_read(__s));
	}

	public function connect(host : Host, port : Int) {
		socket_connect(__s, host, port);
	}

	public function listen(connections : Int) {
		socket_listen(__s, connections);
	}

	public function shutdown( read : Bool, write : Bool ){
		socket_shutdown(__s,read,write);
	}

	public function bind(host : Host, port : Int) {
		socket_bind(__s, host, port);
	}

	public function accept() : Socket {
		var s = socket_accept(__s);
		var so = new Socket();
		so.close();
		so.__s = s;
		return so;
	}

	public function peer() : { host : Host, port : Int } {
		var a = socket_peer(__s);
		return { host : a[0], port : a[1] };
	}

	public function host() : { host : Host, port : Int } {
		var a = socket_host(__s);
		return { host : a[0], port : a[1] };
	}

	public function setTimeout(timeout : Int) {
		socket_set_timeout(__s, timeout);
	}

	public function waitForRead() {
		select([this],null,null,null);
	}

	public function readUntil( end : Int ) : String {
		var buf = new StringBuf();
		var last : Int;
		while( (last = socket_recv_char(__s)) != end )
			buf.addChar( last );
		return buf.toString();
	}

	public function readLine() : String {
		return readUntil( 10 );
	}

	public function setBlocking( b : Bool ) {
		socket_set_blocking(__s,b);
	}

	// STATICS
	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, timeout : Float) : {read: Array<Socket>,write: Array<Socket>,others: Array<Socket>} {
		var c = untyped __dollar__hnew( 1 );
		var f = function( a ){
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

	public static function resolve(host : String) : Host {
		return host_resolve(untyped host.__s);
	}

	public static function hostToString(host : Host) : String {
		return new String(host_to_string(host));
	}

	public static function reverse( host : Host ) : String {
		return new String(host_reverse(host));
	}

	public static function localhost() : String {
		return new String(host_local());
	}

	static function __init__() {
		Lib.load("std","socket_init",0)();
	}

	private static var socket_new = Lib.load("std","socket_new",1);
	private static var socket_close = Lib.load("std","socket_close",1);
	private static var socket_send_char = Lib.load("std","socket_send_char",2);
	private static var socket_send = Lib.load("std","socket_send",4);
	private static var socket_recv = Lib.load("std","socket_recv",4);
	private static var socket_recv_char = Lib.load("std","socket_recv_char",1);
	private static var socket_write = Lib.load("std","socket_write",2);
	private static var socket_read = Lib.load("std","socket_read",1);
	private static var host_resolve = Lib.load("std","host_resolve",1);
	private static var host_reverse = Lib.load("std","host_reverse",1);
	private static var host_to_string = Lib.load("std","host_to_string",1);
	private static var host_local = Lib.load("std","host_local",0);
	private static var socket_connect = Lib.load("std","socket_connect",3);
	private static var socket_listen = Lib.load("std","socket_listen",2);
	private static var socket_select = Lib.load("std","socket_select",4);
	private static var socket_bind = Lib.load("std","socket_bind",3);
	private static var socket_accept = Lib.load("std","socket_accept",1);
	private static var socket_peer = Lib.load("std","socket_peer",1);
	private static var socket_host = Lib.load("std","socket_host",1);
	private static var socket_set_timeout = Lib.load("std","socket_set_timeout",2);
	private static var socket_shutdown = Lib.load("std","socket_shutdown",3);
	private static var socket_set_blocking = Lib.load("std","socket_set_blocking",2);

}
