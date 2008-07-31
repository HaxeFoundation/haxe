/*
 * Copyright (c) 2005-2007, The haXe Project Contributors
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
 */
package haxe.remoting;

typedef Socket =
	#if flash9
		flash.net.XMLSocket
	#elseif flash
		flash.XMLSocket
	#elseif js
		js.XMLSocket
	#elseif neko
		neko.net.Socket
	#elseif php
		php.net.Socket
	#else
		Dynamic
	#end

/**
	The haXe Remoting Socket Protocol is composed of serialized string exchanges.
	Each string is prefixed with a 2-chars header encoding the string size (up to 4KB)
	and postfixed with the \0 message delimiting char.
	A request string is composed of the following serialized values :
		- the boolean true for a request
		- an array of strings representing the object+method path
		- an array of parameters
	A response string is composed of the following serialized values :
		- the boolean false for a response
		- a serialized value representing the result
	Exceptions are serialized with [serializeException] so they will be thrown immediatly
	when they are unserialized.
**/
class SocketProtocol {

	public var socket : Socket;
	public var context : Context;

	public function new( sock, ctx ) {
		this.socket = sock;
		this.context = ctx;
	}

	function decodeChar(c) : Null<Int> {
		// A...Z
		if( c >= 65 && c <= 90 )
			return c - 65;
		// a...z
		if( c >= 97 && c <= 122 )
			return c - 97 + 26;
		// 0...9
		if( c >= 48 && c <= 57 )
			return c - 48 + 52;
		// +
		if( c == 43 )
			return 62;
		// /
		if( c == 47 )
			return 63;
		return null;
	}

	function encodeChar(c) : Null<Int> {
		if( c < 0 )
			return null;
		// A...Z
		if( c < 26 )
			return c + 65;
		// a...z
		if( c < 52 )
			return (c - 26) + 97;
		// 0...9
		if( c < 62 )
			return (c - 52) + 48;
		// +
		if( c == 62 )
			return 43;
		// /
		if( c == 63 )
			return 47;
		return null;
	}

	public function messageLength( c1 : Int, c2 : Int ) : Null<Int> {
		var e1 = decodeChar(c1);
		var e2 = decodeChar(c2);
		if( e1 == null || e2 == null )
			return null;
		return (e1 << 6) | e2;
	}

	public function encodeMessageLength( len : Int ) {
		var c1 = encodeChar(len>>6);
		if( c1 == null )
			throw "Message is too big";
		var c2 = encodeChar(len&63);
		return { c1 : c1, c2 : c2 };
	}

	public function sendRequest( path : Array<String>, params : Array<Dynamic> ) {
		var s = new haxe.Serializer();
		s.serialize(true);
		s.serialize(path);
		s.serialize(params);
		sendMessage(s.toString());
	}

	public function sendAnswer( answer : Dynamic, ?isException : Bool ) {
		var s = new haxe.Serializer();
		s.serialize(false);
		if( isException )
			s.serializeException(answer);
		else
			s.serialize(answer);
		sendMessage(s.toString());
	}

	public function sendMessage( msg : String ) {
		var e = encodeMessageLength(msg.length + 3);
		#if (neko || php)
		var o = socket.output;
		o.writeByte(e.c1);
		o.writeByte(e.c2);
		o.writeString(msg);
		o.writeByte(0);
		#else
		socket.send(String.fromCharCode(e.c1)+String.fromCharCode(e.c2)+msg);
		#end
	}

	public dynamic function decodeData( data : String ) {
		return data;
	}

	public function isRequest( data : String ) {
		return switch( haxe.Unserializer.run(data) ) {
		case true: true;
		case false: false;
		default: throw "Invalid data";
		}
	}

	public function processRequest( data : String, ?onError : Array<String> -> Array<Dynamic> -> Dynamic -> Void ) {
		var s = new haxe.Unserializer(data);
		var result : Dynamic;
		var isException = false;
		if( s.unserialize() != true )
			throw "Not a request";
		var path : Array<String> = s.unserialize();
		var args : Array<Dynamic> = s.unserialize();
		try {
			if( context == null ) throw "No context is shared";
			result = context.call(path,args);
		} catch( e : Dynamic ) {
			result = e;
			isException = true;
		}
		// send back result/exception over network
		sendAnswer(result,isException);
		// send the error event
		if( isException && onError != null )
			onError(path,args,result);
	}

	public function processAnswer( data : String ) : Dynamic {
		var s = new haxe.Unserializer(data);
		if( s.unserialize() != false )
			throw "Not an answer";
		return s.unserialize();
	}

	#if (neko || php)

	public function readMessage() {
		var i = socket.input;
		var c1 = i.readByte();
		var c2 = i.readByte();
		var len = messageLength(c1,c2);
		if( len == null )
			throw "Invalid header";
		var data = i.readString(len - 3);
		if( i.readByte() != 0 )
			throw "Invalid message";
		return decodeData(data);
	}

	#end

}
