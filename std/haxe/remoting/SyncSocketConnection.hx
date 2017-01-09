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

class SyncSocketConnection implements Connection implements Dynamic<Connection> {

	var __path : Array<String>;
	var __proto : SocketProtocol;

	function new(proto,path) {
		__proto = proto;
		__path = path;
	}

	public function resolve( name ) : Connection {
		var s = new SyncSocketConnection(__proto,__path.copy());
		s.__path.push(name);
		return s;
	}

	public function call( params : Array<Dynamic> ) : Dynamic {
		var proto = __proto;
		proto.sendRequest(__path,params);
		while( true ) {
			var data = proto.readMessage();
			if( proto.isRequest(data) ) {
				if( proto.context == null )
					throw "Request received";
				proto.processRequest(data,onRequestError);
				continue;
			}
			return proto.processAnswer(data);
		}
	}

	public function processRequest() {
		if( __proto.context == null )
			throw "Can't process request";
		var data = __proto.readMessage();
		__proto.processRequest(data,onRequestError);
	}

	public function onRequestError( path : Array<String>, args : Array<Dynamic>, exc : Dynamic ) {
	}

	public function setProtocol( p : SocketProtocol ) {
		__proto = p;
	}

	public function getProtocol() : SocketProtocol {
		return __proto;
	}

	public function close() {
		try __proto.socket.close() catch( e : Dynamic ) { };
	}

	public static function create( s : Socket, ?ctx : Context ) {
		return new SyncSocketConnection(new SocketProtocol(s,ctx),[]);
	}

}
