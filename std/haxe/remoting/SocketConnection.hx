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
import haxe.remoting.SocketProtocol.Socket;

/**
	Allows remoting communications over a socket connection
*/
class SocketConnection implements AsyncConnection implements Dynamic<AsyncConnection> {

	var __path : Array<String>;
	var __data : {
		protocol : SocketProtocol,
		results : List<{ onResult : Dynamic -> Void, onError : Dynamic -> Void }>,
		log : Array<String> -> Array<Dynamic> -> Dynamic -> Void,
		error : Dynamic -> Void,
		#if js
		queue : Array<Void -> Void>,
		timer : haxe.Timer,
		#end
	};

	function new(data,path) {
		__data = data;
		__path = path;
	}

	public function resolve(name) : AsyncConnection {
		var s = new SocketConnection(__data,__path.copy());
		s.__path.push(name);
		return s;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) {
		try {
			__data.protocol.sendRequest(__path,params);
			__data.results.add({ onResult : onResult, onError : __data.error });
		} catch( e : Dynamic ) {
			__data.error(e);
		}
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function setErrorLogger(h) {
		__data.log = h;
	}

	public function setProtocol( p : SocketProtocol ) {
		__data.protocol = p;
	}

	public function getProtocol() : SocketProtocol {
		return __data.protocol;
	}

	public function close() {
		try __data.protocol.socket.close() catch( e : Dynamic ) { };
	}

	public function processMessage( data : String ) {
		var request;
		var proto = __data.protocol;
		data = proto.decodeData(data);
		try {
			request = proto.isRequest(data);
		} catch( e : Dynamic ) {
			var msg = Std.string(e) + " (in "+StringTools.urlEncode(data)+")";
			__data.error(msg); // protocol error
			return;
		}
		// request
		if( request ) {
			try proto.processRequest(data,__data.log) catch( e : Dynamic ) __data.error(e);
			return;
		}
		// answer
		var f = __data.results.pop();
		if( f == null ) {
			__data.error("No response excepted ("+data+")");
			return;
		}
		var ret;
		try {
			ret = proto.processAnswer(data);
		} catch( e : Dynamic ) {
			f.onError(e);
			return;
		}
		if( f.onResult != null ) f.onResult(ret);
	}

	function defaultLog(path,args,e) {
		// exception inside the called method
		var astr, estr;
		try astr = args.join(",") catch( e : Dynamic ) astr = "???";
		try estr = Std.string(e) catch( e : Dynamic ) estr = "???";
		var header = "Error in call to "+path.join(".")+"("+astr+") : ";
		__data.error(header + estr);
	}

	public static function create( s : Socket, ?ctx : Context ) {
		var data = {
			protocol : new SocketProtocol(s,ctx),
			results : new List(),
			error : function(e) throw e,
			log : null,
			#if js
			queue : [],
			timer : null,
			#end
		};
		var sc = new SocketConnection(data,[]);
		data.log = sc.defaultLog;
		#if flash
		s.addEventListener(flash.events.DataEvent.DATA, function(e : flash.events.DataEvent) {
			var data = e.data;
			var msgLen = sc.__data.protocol.messageLength(data.charCodeAt(0),data.charCodeAt(1));
			if( msgLen == null || data.length != msgLen - 1 ) {
				sc.__data.error("Invalid message header");
				return;
			}
			sc.processMessage(e.data.substr(2,e.data.length-2));
		});
		#elseif js
		// we can't deliver directly the message
		// since it might trigger a blocking action on JS side
		// and in that case this will trigger a Flash bug
		// where a new onData is called is a parallel thread
		// ...with the buffer of the previous onData (!)
		s.onData = function( data : String ) {
			sc.__data.queue.push(function() {
				var msgLen = sc.__data.protocol.messageLength(data.charCodeAt(0),data.charCodeAt(1));
				if( msgLen == null || data.length != msgLen - 1 ) {
					sc.__data.error("Invalid message header");
					return;
				}
				sc.processMessage(data.substr(2,data.length-2));
			});
			if( sc.__data.timer == null ) {
				sc.__data.timer = new haxe.Timer(1);
				sc.__data.timer.run = function() {
					var q = sc.__data.queue.shift();
					if( q == null ) {
						sc.__data.timer.stop();
						sc.__data.timer = null;
						return;
					}
					q();
				};
			}
		};
		#end
		return sc;
	}

}
