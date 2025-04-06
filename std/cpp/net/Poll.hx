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

package cpp.net;

import sys.net.Socket;
import cpp.NativeSocket;

/**
 * A wrapper around native socket polling functionality for monitoring multiple sockets for I/O readiness.
 * This class provides a high-level abstraction over native `poll` operations, allowing you to monitor sockets for read and write events.
 */
class Poll {
	var mPollHandle:Dynamic;
	
	/**
     	 * An array of indices corresponding to sockets ready for reading after polling.
     	 */
	public var readIndexes:Array<Int>;
	
	/**
     	 * An array of indices corresponding to sockets ready for writing after polling.
     	 */
	public var writeIndexes:Array<Int>;

	/**
    	 * Creates a new `Poll` instance.
     	 * 
     	 * @param n The maximum number of sockets to monitor.
     	 */
	public function new(n:Int) {
		mPollHandle = NativeSocket.socket_poll_alloc(n);
		readIndexes = [];
		writeIndexes = [];
	}

	/**
     	 * Prepares the poll structure for monitoring read and write events on the given sockets.
     	 * 
     	 * @param read  An array of sockets to monitor for readability.
     	 * @param write An array of sockets to monitor for writability.
     	 */
	public function prepare(read:Array<Socket>, write:Array<Socket>) {
		var k = NativeSocket.socket_poll_prepare(mPollHandle, read, write);
		readIndexes = k[0];
		writeIndexes = k[1];
	}

	/**
     	 * Waits for events on the prepared sockets.
     	 * 
     	 * @param t The timeout in seconds. Use `-1.0` for an infinite wait. Defaults to `-1.0` if not specified.
     	 */
	public function events(?t:Float) {
		if (t == null)
			t = -1.0;
		NativeSocket.socket_poll_events(mPollHandle, t);
	}

	/**
     	 * Polls the given sockets for any events (e.g., readability or writability).
     	 * 
     	 * @param a An array of sockets to monitor for events.
     	 * @param t The timeout in seconds. Use `-1.0` for an infinite wait. Defaults to `-1.0` if not specified.
     	 * @return An array of sockets that are ready for I/O operations.
     	 */
	public function poll(a:Array<Socket>, ?t:Float):Array<Socket> {
		if (t == null)
			t = -1.0;
		return NativeSocket.socket_poll(a, mPollHandle, t);
	}
}
