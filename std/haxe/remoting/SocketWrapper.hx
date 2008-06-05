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

/**
	See [js.XMLSocket]
**/
class SocketWrapper {

	static var ID = 0;

	static function create( prefix : String ) : String {
		var id = prefix + "WrappedSocket"+(ID++);
		var s = new Socket();
		var ctx = new Context();
		var cnx = haxe.remoting.ExternalConnection.jsConnect(id,ctx);
		ctx.addObject("sock",s);
		var o = {};
		Reflect.setField(o,"close",cnx.close);
		ctx.addObject("api",o);
		#if flash9
		var connected = false;
		s.addEventListener(flash.events.Event.CONNECT,function(_) {
			connected = true;
			cnx.api.onConnect.call([true]);
		});
		s.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR,function(_) {
			if( connected )
				cnx.api.onClose.call([]);
			else
				cnx.api.onConnect.call([false]);
		});
		s.addEventListener(flash.events.Event.CLOSE,function(_) {
			cnx.api.onClose.call([]);
		});
		s.addEventListener(flash.events.DataEvent.DATA,function(e:flash.events.DataEvent) {
			cnx.api.onData.call([e.data]);
		});
		#elseif flash
		s.onConnect = function(b) {
			cnx.api.onConnect.call([b]);
		};
		s.onData = function(data) {
			cnx.api.onData.call([data]);
		};
		s.onClose = function() {
			cnx.api.onClose.call([]);
		};
		#end
		return id;
	}

	static function init() {
		if( !flash.external.ExternalInterface.available ) return;
		var ctx = new Context();
		var o = {};
		Reflect.setField(o,"create",create);
		ctx.addObject("api",o);
		haxe.remoting.ExternalConnection.jsConnect("SocketWrapper",ctx);
	}

	static var _ = init();

}
