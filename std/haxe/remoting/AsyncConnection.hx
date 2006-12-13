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

class AsyncConnection implements Dynamic<AsyncConnection> {

	var __data : Dynamic;
	var __path : Array<String>;
	var __error : { ref : Dynamic -> Void };
	public var onError(getErrorHandler,setErrorHandler) : Dynamic -> Void;

	function new( data : Dynamic, path ) {
		__data = data;
		__path = path;
		__error = { ref : function(e) { throw e; } };
	}

	function __resolve(field) {
		var s = new AsyncConnection(__data,__path.copy());
		s.__error = __error;
		s.__path.push(field);
		return s;
	}

	function getErrorHandler() {
		return __error.ref;
	}

	function setErrorHandler(f) {
		__error.ref = f;
		return f;
	}

	public function call( params : Array<Dynamic>, onData : Dynamic -> Void ) : Void {
		#if flash
		if( __data.connect ) {
			var me = this;
			var p = params.copy();
			#if flash9
			p.unshift(new flash.net.Responder(
				function(e) { me.__error.ref(e); },
				function(r) { onData(r); }
			));
			#else true
			p.unshift({
				onStatus : function(e) { me.__error.ref(e); },
				onResult : function(r) { onData(r); }
			});
			#end
			p.unshift(__path.join("."));
			__data.call.apply(__data,p);
			return;
		}
		#end
		var h = new haxe.Http(__data);
		var me = this;
		var s = new haxe.Serializer();
		s.serialize(__path);
		s.serialize(params);
		h.setHeader("X-Haxe-Remoting","1");
		h.setParameter("__x",s.toString());
		h.onData = function(data : String) {
			var ok = true;
			var v;
			try {
				if( data.length < 3 || data.substr(0,3) != "hxr" )
					throw "Invalid response : '"+data+"'";
				var s = new haxe.Unserializer(data.substr(3,data.length-3));
				v = s.unserialize();
			} catch( err : Dynamic ) {
				ok = false;
				me.__error.ref(err);
			}
			if( ok )
				onData(v);
		};
		h.onError = function(e) { me.__error.ref(e); };
		h.request(true);
	}

	public static function urlConnect( url : String ) {
		return new AsyncConnection(url,[]);
	}

	#if flash
	public static function amfConnect( gatewayUrl : String ) {
		#if flash9
		var c = new flash.net.NetConnection();
		var cnx = new AsyncConnection(c,[]);
		c.addEventListener(flash.events.NetStatusEvent.NET_STATUS,function(e:flash.events.NetStatusEvent) {
			cnx.onError(e);
		});
		c.connect(gatewayUrl);
		return cnx;
		#else true
		var c = new flash.NetConnection();
		if( !c.connect(gatewayUrl) )
			throw "Could not connected to gateway url "+gatewayUrl;
		return new AsyncConnection(c,[]);
		#end
	}
	#end

}
