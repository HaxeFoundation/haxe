/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
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
 */
package sys.net;

/**
	A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
extern class Socket {

	/**
		The stream on which you can read available data. By default the stream is blocking until the requested data is available,
		use [setBlocking(false)] or [setTimeout] to prevent infinite waiting.
	**/
	var input(default,null) : haxe.io.Input;

	/**
		The stream on which you can send data. Please note that in case the output buffer you will block while writing the data, use [setBlocking(false)] or [setTimeout] to prevent that.
	**/
	var output(default,null) : haxe.io.Output;

	/**
		A custom value that can be associated with the socket. Can be used to retreive your custom infos after a [select].
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
		Connect to the given server host/port. Throw an exception in case we couldn't sucessfully connect.
	**/
	function connect( host : Host, port : Int ) : Void;

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use [accept()] to accept incoming connections.
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
		Return the informations about the other side of a connected socket.
	**/
	function peer() : { host : Host, port : Int };

	/**
		Return the informations about our side of a connected socket.
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
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediatly by throwing a haxe.io.Error.Blocking value.
	**/
	function setBlocking( b : Bool ) : Void;

	/**
		Allows the socket to immediatly send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	function setFastSend( b : Bool ) : Void;

	/**
		Wait until one of the sockets groups is ready for the given operation :
		[read] contains sockets on which we want to wait for available data to be read,
		[write] contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
		[others] contains sockets on which we want to wait for exceptional conditions.
		[select] will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
		In case a [timeout] (in seconds) is specified, select might wait at worse until the timeout expires.
	**/
	static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> };

}
