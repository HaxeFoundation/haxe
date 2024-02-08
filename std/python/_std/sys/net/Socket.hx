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

import haxe.io.Error;
import haxe.io.Bytes;
import haxe.io.BytesData;
import python.Exceptions;
import python.Tuple;
import python.lib.socket.Socket in PSocket;
import python.lib.Socket in PSocketModule;
import python.lib.socket.Address in PAddress;
import python.lib.Select;
import python.lib.ssl.Errors;

private class SocketInput extends haxe.io.Input {
	var __s:PSocket;

	public function new(s) {
		__s = s;
	}

	public override function readByte():Int {
		var r:BytesData;
		try {
			r = __s.recv(1, 0);
		} catch (e:BlockingIOError) {
			throw Blocked;
		}
		if (r.length == 0)
			throw new haxe.io.Eof();
		return python.Syntax.code("r[0]");
	}

	public override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		var r;
		var data = buf.getData();
		try {
			try {
				r = __s.recv(len, 0);
			} catch(e:SSLWantReadError) {
				return 0;
			}
			for (i in pos...(pos + r.length)) {
				data.set(i, r[i - pos]);
			}
		} catch (e:BlockingIOError) {
			throw Blocked;
		}
		if (r.length == 0)
			throw new haxe.io.Eof();
		return r.length;
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}
}

private class SocketOutput extends haxe.io.Output {
	var __s:PSocket;

	public function new(s) {
		__s = s;
	}

	public override function writeByte(c:Int) {
		try {
			__s.send(python.Syntax.code('bytes([c])'), 0);
		} catch (e:BlockingIOError) {
			throw Blocked;
		}
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		try {
			var data = buf.getData();
			var payload = python.Syntax.code("{0}[{1}:{1}+{2}]", data, pos, len);
			var r = 0;
			try {
				r = __s.send(payload, 0);
			} catch(e:SSLWantWriteError) {
				return 0;
			}
			return r;
		} catch (e:BlockingIOError) {
			throw Blocked;
		}
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}
}

@:coreApi class Socket {
	var __s:PSocket;

	public var input(default, null):haxe.io.Input;

	public var output(default, null):haxe.io.Output;

	public var custom:Dynamic;

	public function new():Void {
		__initSocket();
		input = new SocketInput(__s);
		output = new SocketOutput(__s);
	}

	function __initSocket():Void {
		__s = new PSocket();
	}

    function __rebuildIoStreams():Void {
		input = new SocketInput(__s);
		output = new SocketOutput(__s);
    }

	public function close():Void {
		__s.close();
	}

	public function read():String {
		return input.readAll().toString();
	}

	public function write(content:String):Void {
		output.writeString(content);
	}

	public function connect(host:Host, port:Int):Void {
		var host_str = host.toString();
		__s.connect(Tuple2.make(host_str, port));
	}

	public function listen(connections:Int):Void {
		__s.listen(connections);
	}

	public function shutdown(read:Bool, write:Bool):Void
		__s.shutdown((read && write) ? PSocketModule.SHUT_RDWR : read ? PSocketModule.SHUT_RD : PSocketModule.SHUT_WR);

	public function bind(host:Host, port:Int):Void {
		var host_str = host.toString();
		__s.bind(Tuple2.make(host_str, port));
	}

	public function accept():Socket {
		var tp2:Tuple2<PSocket, PAddress> = __s.accept();
		var s = new Socket();
		s.__s = tp2._1;
		s.input = new SocketInput(s.__s);
		s.output = new SocketOutput(s.__s);
		return s;
	}

	public function peer():{host:Host, port:Int} {
		var pn = __s.getpeername();
		return {host: new Host(pn._1), port: pn._2}
	}

	public function host():{host:Host, port:Int} {
		var pn = __s.getsockname();
		return {host: new Host(pn._1), port: pn._2};
	}

	public function setTimeout(timeout:Float):Void {
		__s.settimeout(timeout);
	}

	public function waitForRead():Void {
		Select.select([this], [], []);
	}

	public function setBlocking(b:Bool):Void {
		__s.setblocking(b);
	}

	public function setFastSend(b:Bool):Void {
		__s.setsockopt(PSocketModule.SOL_TCP, PSocketModule.TCP_NODELAY, b);
	}

	@:keep function fileno():Int
		return __s.fileno();

	public static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
			?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>} {
		var t3 = Select.select(read, write, others, timeout);
		return {read: t3._1, write: t3._2, others: t3._3};
	}
}
