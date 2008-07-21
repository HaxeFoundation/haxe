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
import haxe.remoting.SocketProtocol.Socket;

class SyncSocketConnection implements Connection, implements Dynamic<Connection> {

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
		return null; // never reached
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
