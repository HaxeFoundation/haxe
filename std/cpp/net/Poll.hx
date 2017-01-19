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
package cpp.net;
import sys.net.Socket;
import cpp.NativeSocket;

class Poll {

	var mPollHandle : Dynamic;
	public var readIndexes : Array<Int>;
	public var writeIndexes : Array<Int>;

	public function new( n : Int ) {
		mPollHandle = NativeSocket.socket_poll_alloc(n);
		readIndexes = [];
		writeIndexes = [];
	}

	public function prepare( read : Array<Socket>, write : Array<Socket> ) {
		var k = NativeSocket.socket_poll_prepare(mPollHandle,read,write);
		readIndexes = k[0];
		writeIndexes = k[1];
	}

	public function events( ?t : Float ) {
		if (t==null) t=-1.0;
		NativeSocket.socket_poll_events(mPollHandle,t);
	}

	public function poll( a : Array<Socket>, ?t : Float ) : Array<Socket> {
		if (t==null) t=-1.0;
		return NativeSocket.socket_poll(a,mPollHandle,t);
	}

}
