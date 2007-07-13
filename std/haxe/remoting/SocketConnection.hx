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
package haxe.remoting;
import haxe.remoting.SocketProtocol;

class SocketConnection extends AsyncConnection {

	var __funs : List<Dynamic -> Void>;
	#if neko
	var __r : neko.net.RemotingServer;
	#end

	override function __resolve(field) : AsyncConnection {
		var s = new SocketConnection(__data,__path.copy());
		s.__error = __error;
		s.__funs = __funs;
		#if neko
		s.__r = __r;
		#end
		s.__path.push(field);
		return s;
	}

	override public function call( params : Array<Dynamic>, ?onData : Dynamic -> Void ) : Void {
		try {
			getProtocol().sendRequest(__path,params);
			__funs.add(onData);
		} catch( e : Dynamic ) {
			__error.ref(e);
		}
	}

	public function setProtocol( p : SocketProtocol ) {
		__data = p;
	}

	public function getProtocol() : SocketProtocol {
		return __data;
	}

	public function closeConnection() {
		try getProtocol().socket.close() catch( e : Dynamic ) { };
	}

	public function processMessage( data : String ) {
		var request;
		var proto = getProtocol();
		try {
			request = proto.isRequest(data);
		} catch( e : Dynamic ) {
			__error.ref(e); // protocol error
			return;
		}
		// request
		if( request ) {
			try {
				var me = this;
				var eval =
					#if neko
						__r.resolvePath
					#else flash
						function(path:Array<String>) { return flash.Lib.eval(path.join(".")); }
					#else js
						function(path:Array<String>) { return js.Lib.eval(path.join(".")); }
					#end
				;
				proto.processRequest(data,eval,function(path,name,args,e) {
					// exception inside the called method
					var astr, estr;
					try astr = args.join(",") catch( e : Dynamic ) astr = "???";
					try estr = Std.string(e) catch( e : Dynamic ) estr = "???";
					var header = "Error in call to "+path.join(".")+"."+name+"("+astr+") : ";
					me.__error.ref(header + estr);
				});
			} catch( e : Dynamic ) {
				__error.ref(e); // protocol error or invalid object/method
			}
			return;
		}
		// answer
		var f, v;
		try {
			if( __funs.isEmpty() )
				throw "No response excepted ("+data+")";
			f = __funs.pop();
			v = proto.decodeAnswer(data);
		} catch( e : Dynamic ) {
			__error.ref(e); // protocol error or answer exception
			return;
		}
		if( f != null )
			f(v); // answer callback exception
	}

	#if neko

	public static function socketConnect( s : neko.net.Socket, r : neko.net.RemotingServer ) {
		var sc = new SocketConnection(new SocketProtocol(s),[]);
		sc.__funs = new List();
		sc.__r = r;
		return sc;
	}

	#else (flash || js)

	public static function socketConnect( s : Socket ) {
		var sc = new SocketConnection(new SocketProtocol(s),[]);
		sc.__funs = new List();
		#if flash9
		s.addEventListener(flash.events.DataEvent.DATA, function(e : flash.events.DataEvent) {
			var data = e.data;
			var msgLen = sc.getProtocol().messageLength(data.charCodeAt(0),data.charCodeAt(1));
			if( msgLen == null || data.length != msgLen - 1 ) {
				sc.__error.ref("Invalid message header");
				return;
			}
			sc.processMessage(e.data.substr(2,e.data.length-2));
		});
		#else true
		// we can't deliver directly the message
		// since it might trigger a blocking action on JS side
		// and in that case this will trigger a Flash bug
		// where a new onData is called is a parallel thread
		// ...with the buffer of the previous onData (!)
		s.onData = function(data : String) {
			haxe.Timer.queue(function() {
				var msgLen = sc.getProtocol().messageLength(data.charCodeAt(0),data.charCodeAt(1));
				if( msgLen == null || data.length != msgLen - 1 ) {
					sc.__error.ref("Invalid message header");
					return;
				}
				sc.processMessage(data.substr(2,data.length-2));
			});
		};
		#end
		return sc;
	}

	#else error
	#end

}
