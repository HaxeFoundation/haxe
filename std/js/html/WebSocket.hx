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

// This file is generated from mozilla\WebSocket.webidl. Do not edit!

package js.html;

/**
	The `WebSocket` object provides the API for creating and managing a WebSocket connection to a server, as well as for sending and receiving data on the connection.

	Documentation [WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
**/
@:native("WebSocket")
extern class WebSocket extends EventTarget {
	static inline var CONNECTING : Int = 0;
	static inline var OPEN : Int = 1;
	static inline var CLOSING : Int = 2;
	static inline var CLOSED : Int = 3;
	
	
	/**
		The absolute URL of the WebSocket.
	**/
	var url(default,null) : String;
	
	/**
		The current state of the connection.
	**/
	var readyState(default,null) : Int;
	
	/**
		The number of bytes of queued data.
	**/
	var bufferedAmount(default,null) : Int;
	
	/**
		An event listener to be called when the connection is opened.
	**/
	var onopen : haxe.Constraints.Function;
	
	/**
		An event listener to be called when an error occurs.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		An event listener to be called when the connection is closed.
	**/
	var onclose : haxe.Constraints.Function;
	
	/**
		The extensions selected by the server.
	**/
	var extensions(default,null) : String;
	
	/**
		The sub-protocol selected by the server.
	**/
	var protocol(default,null) : String;
	
	/**
		An event listener to be called when a message is received from the server.
	**/
	var onmessage : haxe.Constraints.Function;
	
	/**
		The binary data type used by the connection.
	**/
	var binaryType : BinaryType;
	
	/** @throws DOMError */
	@:overload( function( url : String ) : Void {} )
	@:overload( function( url : String, protocols : String ) : Void {} )
	function new( url : String, protocols : Array<String> ) : Void;
	/** @throws DOMError */
	function close( ?code : Int, ?reason : String ) : Void;
	/** @throws DOMError */
	@:overload( function( data : String ) : Void {} )
	@:overload( function( data : Blob ) : Void {} )
	@:overload( function( data : js.lib.ArrayBuffer ) : Void {} )
	function send( data : js.lib.ArrayBufferView ) : Void;
}