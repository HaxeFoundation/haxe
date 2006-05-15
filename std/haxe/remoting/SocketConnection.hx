/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package haxe;

class SocketConnection extends AsyncConnection {

	var __funs : List<Dynamic -> Void>;
	#if neko
	var __r : neko.remoting.Server;
	#end

	function __resolve(field) : AsyncConnection {
		var s = new SocketConnection(__data,__path.copy());
		s.onError = onError;
		s.__funs = __funs;
		#if neko
		s.__r = __r;
		#end
		s.__path.push(field);
		return s;
	}

	public function eval( onData : Dynamic -> Void ) : Void {
		var s = new haxe.Serializer();
		s.serialize(true);
		s.serialize(true);
		s.serialize(__path);
		sendMessage(__data,s.toString());
		__funs.add(onData);
	}

	public function call( params : Array<Dynamic>, onData : Dynamic -> Void ) : Void {
		var s = new haxe.Serializer();
		s.serialize(true);
		s.serialize(false);
		s.serialize(__path);
		s.serialize(params);
		sendMessage(__data,s.toString());
		__funs.add(onData);
	}

	static function decodeChar(c) {
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

	static function encodeChar(c) {
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

	static function sendMessage( __data : Dynamic, msg : String ) {
		var len = msg.length + 3;
		var c1 = encodeChar(len>>6);
		if( c1 == null )
			throw "Message is too big";
		var c2 = encodeChar(len&63);
		#if neko
		var s : neko.Socket = __data;
		s.sendChar(c1);
		s.sendChar(c2);
		s.write(msg);
		s.sendChar(0);
		#else flash
		var s : flash.XMLSocket = __data;
		s.send(Std.chr(c1)+Std.chr(c2)+msg);
		#else error
		#end
	}

	public static function processMessage( sc : SocketConnection, data : String ) {
		var f : Dynamic -> Void;
		var val : Dynamic;
		var s = new Unserializer(data);
		try {
			var isrequest : Bool = s.unserialize();
			if( !isrequest ) {
				if( sc.__funs.isEmpty() )
					throw "No response expected";
				f = sc.__funs.pop();
				val = s.unserialize();
				if( f == null )
					return null;
			}
		} catch( e : Dynamic ) {
			sc.onError(e);
			return null;
		}
		if( f != null ) {
			try {
				f(val);
				return null;
			} catch( val : Dynamic ) {
				return { exc : null };
			}
		}
		// ---------------------------
		var exc = false;
		try {
			var iseval : Bool = s.unserialize();
			var path : Array<String> = s.unserialize();
			if( iseval ) {
				#if flash
				val = flash.Lib.eval(path.join("."));
				#else neko
				val = sc.__r.resolvePath(path);
				#else error
				#end
			} else {
				var args = s.unserialize();
				var fname = path.pop();
				#if flash
				var obj = flash.Lib.eval(path.join("."));
				#else neko
				var obj = sc.__r.resolvePath(path);
				#else error
				#end
				var fptr = Reflect.field(obj,fname);
				if( !Reflect.isFunction(fptr) )
					throw "Calling not-a-function '"+fname+"'";
				val = Reflect.callMethod(obj,fptr,args);
			}
		} catch( e : Dynamic ) {
			val = e;
			exc = true;
		}
		try {
			var s = new Serializer();
			s.serialize(false);
			if( exc )
				s.serializeException(val);
			else
				s.serialize(val);
			sendMessage(sc.__data,s.toString());
		} catch( e : Dynamic ) {
			sc.onError(e);
			return null;
		}
		if( exc )
			return { exc : val };
		return null;
	}

	#if neko

	public static function socketConnect( s : neko.Socket, r : neko.remoting.Server ) {
		var sc = new SocketConnection(s,[]);
		sc.__funs = new List();
		sc.__r = r;
		return sc;
	}

	#else flash

	public static function socketConnect( s : flash.XMLSocket ) {
		var sc = new SocketConnection(s,[]);
		sc.__funs = new List();
		// we can't deliver directly the message
		// since it might trigger a blocking action on JS side
		// and in that case this will trigger a Flash bug
		// where a new onData is called is a parallel thread
		// ...with the buffer of the previous onData (!)
		s.onData = function(data : String) {
			var t = new haxe.Timer(0);
			t.run = function() {
				t.stop();
				var e = processMessage(sc,data.substr(2,data.length-2));
				if( e != null )
					throw e.exc;
			};
		};
		return sc;
	}

	#else error
	#end

}
