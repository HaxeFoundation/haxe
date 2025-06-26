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

package python.lib.socket;

import haxe.io.BytesData;
import python.Tuple;

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
@:pythonImport("socket", "socket")
extern class Socket {
	function send(d:BytesData, flags:Int):Int;
	function recv(n:Int, flags:Int):BytesData;

	/**
		Creates a new unconnected socket.
	**/
	function new():Void;

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	function close():Void;

	/**
		Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
	**/
	function connect(addr:python.lib.socket.Address):Void;

	// function create_connection() :

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
	**/
	function listen(connections:Int):Void;

	/**
		Shutdown the socket, either for reading or writing.
	**/
	function shutdown(how:Int):Void;

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	function bind(address:python.lib.socket.Address):Void;

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data.
	**/
	function accept():Tuple2<Socket, Address>;

	/**
		Return the information about the other side of a connected socket.
	**/
	function getpeername():python.lib.socket.Address;

	/**
		Return the information about our side of a connected socket.
	**/
	function getsockname():python.lib.socket.Address;

	/**
		Return the timeout for the socket or null if no timeout has been set.
	**/
	function gettimeout():Null<Float>;

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	function settimeout(timeout:Float):Void;

	/**
		Block until some data is available for read on the socket.
	**/
	function waitForRead():Void;

	/**
		Return the current blocking mode of the socket.
	**/
	@:require(python_version >= 3.7)
	function getblocking():Bool;

	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocked value.
	**/
	function setblocking(b:Bool):Void;

	/**
		Return the current value of the sockopt as an Int or a Bytes buffer (if buflen is specified).
	**/
	function getsockopt(family:Int, option:Int):Int;

	/**
		Change the value of the given socket option.
	**/
	@:overload(function(family:Int, option:Int, value:Int):Void {})
	function setsockopt(family:Int, option:Int, value:Bool):Void;

	function fileno():Int;
	/**
		Wait until one of the sockets groups is ready for the given operation :
		- `read` contains sockets on which we want to wait for available data to be read,
		- `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		- `others` contains sockets on which we want to wait for exceptional conditions.
		- `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	// static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> };
}
