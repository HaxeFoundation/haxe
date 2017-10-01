/*
 * Copyright (C)2015 Haxe Foundation
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
package neko.net;
import sys.net.Socket;

class Epoll {

	public static inline var EPOLLIN = 0x001;
	public static inline var EPOLLPRI = 0x002;
	public static inline var EPOLLOUT = 0x004;
	public static inline var EPOLLRDNORM = 0x040;
	public static inline var EPOLLRDBAND = 0x080;
	public static inline var EPOLLWRNORM = 0x100;
	public static inline var EPOLLWRBAND = 0x200;
	public static inline var EPOLLMSG = 0x400;
	public static inline var EPOLLERR = 0x008;
	public static inline var EPOLLHUP = 0x010;
	public static inline var EPOLLET = (1 << 31);

	var d : Dynamic;
	var fds : Map<Int, Socket>;
	var result : Array<Socket>;

	public function new( maxEvents : Int ) {
		d = socket_epoll_alloc(maxEvents);
		fds = new Map();
		result = new Array();
	}

	public function register( s : Socket, events : UInt = (EPOLLIN | EPOLLPRI) ) : Void {
		var fd:Int = socket_epoll_register(d, untyped s.__s, events);
		fds.set(fd, s);
	}

	public function unregister( s : Socket ) : Void {
		var fd:Int = socket_epoll_unregister(d, untyped s.__s);
		fds.remove(fd);
	}

	public function wait( ?timeout : Float ) : Array<Socket> {
		var events:Array<Dynamic> = socket_epoll_wait(d, timeout);
		var len = untyped __dollar__asize(events);
		result.splice(0, result.length);
		for (i in 0 ... len)
			result.push(fds.get(events[i]));
		return result;
	}

	static var socket_epoll_alloc = neko.Lib.load("std","socket_epoll_alloc",1);
	static var socket_epoll_register = neko.Lib.load("std","socket_epoll_register",3);
	static var socket_epoll_unregister = neko.Lib.load("std","socket_epoll_unregister",2);
	static var socket_epoll_wait = neko.Lib.load("std","socket_epoll_wait",2);

}
