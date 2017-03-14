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
package haxe.remoting;

typedef Socket =
	#if flash
		flash.net.XMLSocket
	#elseif js
		js.XMLSocket
	#elseif sys
		sys.net.Socket
	#else
		Dynamic
	#end

/**
	The Haxe Remoting Socket Protocol is composed of serialized string exchanges.
	Each string is prefixed with a 2-chars header encoding the string size (up to 4KB)
	and postfixed with the `\0` message delimiting char.

	A request string is composed of the following serialized values :

	 - the boolean true for a request
	 - an array of strings representing the object+method path
	 - an array of parameters

	A response string is composed of the following serialized values :

	 - the boolean false for a response
	 - a serialized value representing the result

	Exceptions are serialized with `serializeException` so they will be thrown immediately
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
		#if sys
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

	#if sys

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
