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

// This file is generated from mozilla\WebSocket.webidl. Do not edit!

package js.html;

/**
	The `WebSocket` object provides the API for creating and managing a WebSocket connection to a server, as well as for sending and receiving data on the connection.

	Documentation [WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
**/
@:native("WebSocket")
extern class WebSocket extends EventTarget
{
	static inline var CONNECTING : Int = 0;
	static inline var OPEN : Int = 1;
	static inline var CLOSING : Int = 2;
	static inline var CLOSED : Int = 3;
	
	var url(default,null) : String;
	var readyState(default,null) : Int;
	var bufferedAmount(default,null) : Int;
	var onopen : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onclose : haxe.Constraints.Function;
	var extensions(default,null) : String;
	var protocol(default,null) : String;
	var onmessage : haxe.Constraints.Function;
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
	@:overload( function( data : ArrayBuffer ) : Void {} )
	function send( data : ArrayBufferView ) : Void;
}