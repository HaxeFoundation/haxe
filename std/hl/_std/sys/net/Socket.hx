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
import haxe.io.Error;

#if doc_gen
@:noDoc enum SocketHandle { }
#else
@:noDoc typedef SocketHandle = hl.Abstract<"hl_socket">;
#end

private class SocketOutput extends haxe.io.Output {

	var sock : Socket;

	public function new(s) {
		this.sock = s;
	}

	public override function writeByte( c : Int ) {
		var k = socket_send_char(@:privateAccess sock.__s, c);
		if( k < 0 ) {
			if( k == -1 ) throw Blocked;
			throw new haxe.io.Eof();
		}
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw haxe.io.Error.OutsideBounds;
		var n = socket_send(@:privateAccess sock.__s, buf.getData().bytes, pos, len);
		if( n < 0 ) {
			if( n == -1 ) throw Blocked;
			throw new haxe.io.Eof();
		}
		return n;
	}

	public override function close() {
		sock.close();
	}


	@:hlNative("std","socket_send_char") static function socket_send_char( s : SocketHandle, c : Int ) : Int { return 0; }
	@:hlNative("std","socket_send") static function socket_send( s : SocketHandle, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }

}

private class SocketInput extends haxe.io.Input {

	var sock : Socket;

	public function new(s) {
		sock = s;
	}

	public override function readByte() : Int {
		var c = socket_recv_char(@:privateAccess sock.__s);
		if( c < 0 ) {
			if( c == -1 ) throw Blocked;
			throw new haxe.io.Eof();
		}
		return c;
	}

	public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		if( pos < 0 || len < 0 || pos + len > buf.length ) throw haxe.io.Error.OutsideBounds;
		var r = socket_recv(@:privateAccess sock.__s,buf.getData().bytes,pos,len);
		if( r < 0 ) {
			if( r == -1 ) throw Blocked;
			throw new haxe.io.Eof();
		}
		return r;
	}

	public override function close() {
		sock.close();
	}

	@:hlNative("std","socket_recv") static function socket_recv( s : SocketHandle, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }
	@:hlNative("std","socket_recv_char") static function socket_recv_char( s : SocketHandle ) : Int { return 0; }

}

@:coreApi
@:keepInit
class Socket {

	private var __s : SocketHandle;
	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;
	public var custom : Dynamic;

	static function __init__() : Void {
		socket_init();
	}

	public function new() : Void {
		init();
	}

	function init() : Void {
		__s = socket_new(false);
		input = new SocketInput(this);
		output = new SocketOutput(this);
	}

	public function close() : Void {
		if( __s != null ) {
			socket_close(__s);
			__s = null;
		}
	}

	public function read() : String {
		return input.readAll().toString();
	}

	public function write( content : String ) : Void {
		output.writeString(content);
	}

	public function connect(host : Host, port : Int) : Void {
		if( !socket_connect(__s, host.ip, port) )
			throw new Sys.SysError("Failed to connect on "+host.toString()+":"+port);
	}

	public function listen( connections : Int ) : Void {
		if( !socket_listen(__s, connections) )
			throw new Sys.SysError("listen() failure");
	}

	public function shutdown( read : Bool, write : Bool ) : Void {
		if( !socket_shutdown(__s, read, write) )
			throw new Sys.SysError("shutdown() failure");
	}

	public function bind(host : Host, port : Int) : Void {
		if( !socket_bind(__s, host.ip, port) )
			throw new Sys.SysError("Cannot bind socket on " + host + ":" + port);
	}

	public function accept() : Socket {
		var c = socket_accept(__s);
		if( c == null )
			return null;
		var s : Socket = untyped $new(Socket);
		s.__s = c;
		s.input = new SocketInput(s);
		s.output = new SocketOutput(s);
		return s;
	}

	public function peer() : { host : Host, port : Int } {
		var ip = 0, port = 0;
		if( !socket_peer(__s, ip, port) )
			return null;
		var h : Host = untyped $new(Host);
		@:privateAccess h.ip = ip;
		return { host : h, port : port };
	}

	public function host() : { host : Host, port : Int } {
		var ip = 0, port = 0;
		if( !socket_host(__s, ip, port) )
			return null;
		var h : Host = untyped $new(Host);
		@:privateAccess h.ip = ip;
		return { host : h, port : port };
	}

	public function setTimeout( timeout : Float ) : Void {
		if( !socket_set_timeout(__s, timeout) ) throw new Sys.SysError("setTimeout() failure");
	}

	public function waitForRead() : Void {
		select([this],null,null,null);
	}

	public function setBlocking( b : Bool ) : Void {
		if( !socket_set_blocking(__s, b) ) throw new Sys.SysError("setBlocking() failure");
	}

	public function setFastSend( b : Bool ) : Void {
		if( !socket_set_fast_send(__s,b) ) throw new Sys.SysError("setFastSend() failure");
	}

	// TODO : use TLS when multithread added
	static var tmp : hl.Bytes = null;
	static var curTmpSize = 0;

	static function makeArray( a : Array<Socket> ) : hl.NativeArray<SocketHandle> {
		if( a == null ) return null;
		var arr = new hl.NativeArray(a.length);
		for( i in 0...a.length )
			arr[i] = a[i].__s;
		return arr;
	}

	static function outArray( a : hl.NativeArray<SocketHandle>, original : Array<Socket> ) : Array<Socket> {
		var out = [];
		if( a == null ) return out;
		var i = 0, p = 0;
		var max = original.length;
		while( i < max ) {
			var sh = a[i++];
			if( sh == null ) break;
			while( original[p].__s != sh ) p++;
			out.push(original[p++]);
		}
		return out;
	}

	public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : {read: Array<Socket>, write: Array<Socket>, others: Array<Socket>} {
		var sread = makeArray(read);
		var swrite = makeArray(write);
		var sothers = makeArray(others);
		var tmpSize = 0;
		if( sread != null ) tmpSize += socket_fd_size(sread.length);
		if( swrite != null ) tmpSize += socket_fd_size(swrite.length);
		if( sothers != null ) tmpSize += socket_fd_size(sothers.length);
		if( tmpSize > curTmpSize ) {
			tmp = new hl.Bytes(tmpSize);
			curTmpSize = tmpSize;
		}
		if( !socket_select(sread, swrite, sothers, tmp, curTmpSize, timeout == null ? -1 : timeout) )
			throw "Error while waiting on socket";
		return {
			read : outArray(sread,read),
			write : outArray(swrite,write),
			others : outArray(sothers,others),
		};
	}

	@:hlNative("std", "socket_init") static function socket_init() : Void {}
	@:hlNative("std", "socket_new") static function socket_new( udp : Bool ) : SocketHandle { return null; }
	@:hlNative("std", "socket_close") static function socket_close( s : SocketHandle ) : Void { }
	@:hlNative("std", "socket_connect") static function socket_connect( s : SocketHandle, host : Int, port : Int ) : Bool { return true; }
	@:hlNative("std", "socket_listen") static function socket_listen( s : SocketHandle, count : Int ) : Bool { return true; }
	@:hlNative("std", "socket_bind") static function socket_bind( s : SocketHandle, host : Int, port : Int ) : Bool { return true; }
	@:hlNative("std", "socket_accept") static function socket_accept( s : SocketHandle ) : SocketHandle { return null; }
	@:hlNative("std", "socket_peer") static function socket_peer( s : SocketHandle, host : hl.Ref<Int>, port : hl.Ref<Int> ) : Bool { return true; }
	@:hlNative("std", "socket_host") static function socket_host( s : SocketHandle, host : hl.Ref<Int>, port : hl.Ref<Int> ) : Bool { return true; }
	@:hlNative("std", "socket_set_timeout") static function socket_set_timeout( s : SocketHandle, timeout : Float ) : Bool { return true; }
	@:hlNative("std", "socket_shutdown") static function socket_shutdown( s : SocketHandle, read : Bool, write : Bool ) : Bool { return true; }
	@:hlNative("std", "socket_set_blocking") static function socket_set_blocking( s : SocketHandle, b : Bool ) : Bool { return true; }
	@:hlNative("std", "socket_set_fast_send") static function socket_set_fast_send( s : SocketHandle, b : Bool ) : Bool { return true; }

	@:hlNative("std", "socket_fd_size") static function socket_fd_size( count : Int) : Int { return 0; }
	@:hlNative("std", "socket_select") static function socket_select( read : hl.NativeArray<SocketHandle>, write : hl.NativeArray<SocketHandle>, other : hl.NativeArray<SocketHandle>, tmpData : hl.Bytes, tmpSize : Int, timeout : Float ) : Bool { return false; }

}
