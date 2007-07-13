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
import haxe.remoting.SocketProtocol;

class NekoSocketConnection extends Connection {

	var __r : neko.net.RemotingServer;

	override function __resolve(field) : Connection {
		var s = new NekoSocketConnection(__data,__path.copy());
		s.__r = __r;
		s.__path.push(field);
		return s;
	}

	override public function call( params : Array<Dynamic> ) : Dynamic {
		var proto = getProtocol();
		proto.sendRequest(__path,params);
		while( true ) {
			var data = proto.readMessage();
			if( proto.isRequest(data) ) {
				if( __r == null )
					throw "Request received";
				proto.processRequest(data,__r.resolvePath,onRequestError);
				continue;
			}
			return proto.decodeAnswer(data);
		}
		return null;
	}

	public function processRequest() {
		var proto = getProtocol();
		if( __r == null )
			throw "No RemotingServer defined";
		var data = proto.readMessage();
		proto.processRequest(data,__r.resolvePath,onRequestError);
	}

	public function onRequestError( path : Array<String>, method : String, args : Array<Dynamic>, exc : Dynamic ) {
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

	public static function socketConnect( s : neko.net.Socket, ?r : neko.net.RemotingServer ) {
		var sc = new NekoSocketConnection(new SocketProtocol(s),[]);
		sc.__r = r;
		return sc;
	}

}
