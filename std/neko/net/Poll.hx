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
package neko.net;
import sys.net.Socket;

class Poll {

	var d : Dynamic;
	public var readIndexes : ArrayAccess<Int>;
	public var writeIndexes : ArrayAccess<Int>;

	public function new( n : Int ) {
		d = socket_poll_alloc(n);
		readIndexes = writeIndexes = untyped __dollar__array(-1);
	}

	public function prepare( read : Array<Socket>, write : Array<Socket> ) {
		untyped {
			var r = __dollar__amake(read.length);
			var w = __dollar__amake(write.length);
			var i = 0;
			var len = read.length;
			while( i < len ) {
				r[i] = read[i].__s;
				i += 1;
			}
			i = 0;
			len = write.length;
			while( i < len ) {
				w[i] = write[i].__s;
				i += 1;
			}
			var k = socket_poll_prepare(d,r,w);
			readIndexes = k[0];
			writeIndexes = k[1];
		}
	}

	public function events( ?t : Float ) {
		socket_poll_events(d,t);
	}

	public function poll( a : Array<Socket>, ?t : Float ) : Array<Socket> {
		untyped {
			var c = __dollar__hnew(16);
			var r = neko.NativeArray.alloc(a.length);
			var i = 0;
			var len = a.length;
			while( i < len ){
				r[i] = a[i].__s;
				__dollar__hadd(c,a[i].__s,a[i]);
				i += 1;
			}
			r = socket_poll(r,d,t);
			i = 0;
			len = __dollar__asize(r);
			while( i < len ) {
				r[i] = __dollar__hget(c,r[i],null);
				i += 1;
			}
			return Array.new1(r,len);
		}
	}

	static var socket_poll_alloc = neko.Lib.load("std","socket_poll_alloc",1);
	static var socket_poll = neko.Lib.load("std","socket_poll",3);
	static var socket_poll_prepare = neko.Lib.loadLazy("std","socket_poll_prepare",3);
	static var socket_poll_events = neko.Lib.loadLazy("std","socket_poll_events",2);

}
