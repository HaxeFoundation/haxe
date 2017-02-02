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

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
extern class Socket {

	/**
		The stream on which you can read available data. By default the stream is blocking until the requested data is available,
		use `setBlocking(false)` or `setTimeout` to prevent infinite waiting.
	**/
	var input(default,null) : haxe.io.Input;

	/**
		The stream on which you can send data. Please note that in case the output buffer you will block while writing the data, use `setBlocking(false)` or `setTimeout` to prevent that.
	**/
	var output(default,null) : haxe.io.Output;

	/**
		A custom value that can be associated with the socket. Can be used to retrieve your custom infos after a `select`.
	***/
	var custom : Dynamic;

	/**
		Creates a new unconnected socket.
	**/
	function new() : Void;

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	function close() : Void;

	/**
		Read the whole data available on the socket.
	**/
	function read() : String;

	/**
		Write the whole data to the socket output.
	**/
	function write( content : String ) : Void;

	/**
		Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
	**/
	function connect( host : Host, port : Int ) : Void;

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
	**/
	function listen( connections : Int ) : Void;

	/**
		Shutdown the socket, either for reading or writing.
	**/
	function shutdown( read : Bool, write : Bool ) : Void;

	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	function bind( host : Host, port : Int ) : Void;

	/**
		Accept a new connected client. This will return a connected socket on which you can read/write some data.
	**/
	function accept() : Socket;

	/**
		Return the information about the other side of a connected socket.
	**/
	function peer() : { host : Host, port : Int };

	/**
		Return the information about our side of a connected socket.
	**/
	function host() : { host : Host, port : Int };

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	function setTimeout( timeout : Float ) : Void;

	/**
		Block until some data is available for read on the socket.
	**/
	function waitForRead() : Void;

	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocking value.
	**/
	function setBlocking( b : Bool ) : Void;

	/**
		Allows the socket to immediately send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	function setFastSend( b : Bool ) : Void;

	/**
		Wait until one of the sockets groups is ready for the given operation :
		 - `read`contains sockets on which we want to wait for available data to be read,
		 - `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		 - `others` contains sockets on which we want to wait for exceptional conditions.
		 - `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> };

}
