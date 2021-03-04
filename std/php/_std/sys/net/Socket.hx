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

package sys.net;

import php.*;
import php.Global.*;
import php.Const.*;
import sys.io.FileInput;
import sys.io.FileOutput;

@:coreApi
class Socket {
	private var __s:Resource;
	private var stream:Resource;

	public var input(default, null):haxe.io.Input;
	public var output(default, null):haxe.io.Output;
	public var custom:Dynamic;

	var protocol:String;

	public function new():Void {
		input = @:privateAccess new sys.io.FileInput(null);
		output = @:privateAccess new sys.io.FileOutput(null);
		initSocket();
		protocol = "tcp";
	}

	private function initSocket():Void {
		__s = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
	}

	private function assignHandler():Void {
		stream = socket_export_stream(__s);
		@:privateAccess (cast input : FileInput).__f = stream;
		@:privateAccess (cast output : FileOutput).__f = stream;
	}

	public function close():Void {
		socket_close(__s);
		@:privateAccess (cast input : FileInput).__f = null;
		@:privateAccess (cast output : FileOutput).__f = null;
		input.close();
		output.close();
	}

	public function read():String {
		var b = '';
		while (!feof(stream))
			b += fgets(stream);
		return b;
	}

	public function write(content:String):Void {
		fwrite(stream, content);
	}

	public function connect(host:Host, port:Int):Void {
		var r = socket_connect(__s, host.host, port);
		checkError(r, 0, 'Unable to connect');
		assignHandler();
	}

	public function listen(connections:Int):Void {
		var r = socket_listen(__s, connections);
		checkError(r, 0, 'Unable to listen on socket');
		assignHandler();
	}

	public function shutdown(read:Bool, write:Bool):Void {
		var rw = read && write ? 2 : (write ? 1 : (read ? 0 : 2));
		var r = socket_shutdown(__s, rw);
		checkError(r, 0, 'Unable to shutdown');
	}

	public function bind(host:Host, port:Int):Void {
		var r = socket_bind(__s, host.host, port);
		checkError(r, 0, "Unable to bind socket");
	}

	public function accept():Socket {
		var r = socket_accept(__s);
		checkError(r, 0, 'Unable to accept connections on socket');
		var s = new Socket();
		// FIXME: wasted resource here
		s.__s = r;
		s.assignHandler();
		return s;
	}

	public function peer():{host:Host, port:Int} {
		var host:String = "", port:Int = 0;
		var r = socket_getpeername(__s, host, port);
		checkError(r, 0, 'Unable to retrieve the peer name');
		return {host: new Host(host), port: port};
	}

	public function host():{host:Host, port:Int} {
		var host:String = "", port:Int = 0;
		var r = socket_getsockname(__s, host, port);
		checkError(r, 0, 'Unable to retrieve the host name');
		return {host: new Host(host), port: port};
	}

	public function setTimeout(timeout:Float):Void {
		var s = Std.int(timeout);
		var ms = Std.int((timeout - s) * 1000000);
		var timeOut:NativeStructArray<{sec:Int, usec:Int}> = {sec: s, usec: ms};
		var r = socket_set_option(__s, SOL_SOCKET, SO_RCVTIMEO, timeOut);
		checkError(r, 0, 'Unable to set receive timeout');
		r = socket_set_option(__s, SOL_SOCKET, SO_SNDTIMEO, timeOut);
		checkError(r, 0, 'Unable to set send timeout');
	}

	public function setBlocking(b:Bool):Void {
		var r = b ? socket_set_block(__s) : socket_set_nonblock(__s);
		checkError(r, 0, 'Unable to set blocking');
	}

	public function setFastSend(b:Bool):Void {
		var r = socket_set_option(__s, SOL_TCP, TCP_NODELAY, true);
		checkError(r, 0, "Unable to set TCP_NODELAY on socket");
	}

	public function waitForRead():Void {
		select([this], null, null);
	}

	private static function checkError(r:Bool, code:Int, msg:String):Void {
		if (r != false)
			return;
		throw haxe.io.Error.Custom('Error [$code]: $msg');
	}


	/**
		Since PHP 8 sockets are represented as instances of class \Socket

		TODO:
			rewrite without `cast` after resolving https://github.com/HaxeFoundation/haxe/issues/9964
	*/
	static inline function getSocketId(s:Resource):Int {
		return PHP_VERSION_ID < 80000 ? Syntax.int(s) : spl_object_id(cast s);
	}

	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var map:Map<Int, Socket> = new Map();
		inline function addSockets(sockets:Array<Socket>) {
			if (sockets != null)
				for (s in sockets)
					map[getSocketId(s.__s)] = s;
		}
		inline function getRaw(sockets:Array<Socket>):Array<Resource> {
			return sockets == null ? [] : [for (s in sockets) s.__s];
		}
		inline function getOriginal(result:Array<Resource>) {
			return [for (r in result) map[getSocketId(r)]];
		}

		addSockets(read);
		addSockets(write);
		addSockets(others);

		// unwrap Sockets into Resources
		var rawRead:NativeIndexedArray<Resource> = getRaw(read),
			rawWrite:NativeIndexedArray<Resource> = getRaw(write),
			rawOthers:NativeIndexedArray<Resource> = getRaw(others);
		var sec = timeout == null ? null : Std.int(timeout);
		var usec = timeout == null ? 0 : Std.int((timeout % 1) * 1000000);
		var result = socket_select(rawRead, rawWrite, rawOthers, sec, usec);
		checkError(result, 0, "Error during select call");
		// convert raw resources back to Socket objects
		return {
			read: getOriginal(rawRead),
			write: getOriginal(rawWrite),
			others: getOriginal(rawOthers),
		}
	}
}
