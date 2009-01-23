/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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

class AMFConnection implements AsyncConnection, implements Dynamic<AsyncConnection> {

	var __data : {
		error : Dynamic -> Void,
		#if flash9
		cnx : flash.net.NetConnection,
		#elseif flash
		cnx : flash.NetConnection,
		#else
		cnx : Dynamic,
		#end
	};
	var __path : Array<String>;

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	public function resolve( name ) : AsyncConnection {
		var s = new AMFConnection(__data,__path.copy());
		s.__path.push(name);
		return s;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function close() {
		__data.cnx.close();
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) : Void {
		if( onResult == null ) onResult = function(e) {};
		var p = params.copy();
		#if flash9
		p.unshift(new flash.net.Responder(onResult,__data.error));
		#else
		p.unshift({ onStatus : __data.error, onResult : onResult });
		#end
		p.unshift(__path.join("."));
		untyped __data.cnx.call.apply(__data,p);
	}

	#if flash
	public static function urlConnect( gatewayUrl : String ) {
		#if flash9
		var c = new flash.net.NetConnection();
		var cnx = new AMFConnection({ cnx : c, error : function(e) throw e },[]);
		c.addEventListener(flash.events.NetStatusEvent.NET_STATUS,function(e:flash.events.NetStatusEvent) {
			cnx.__data.error(e);
		});
		c.connect(gatewayUrl);
		return cnx;
		#else
		var c = new flash.NetConnection();
		if( !c.connect(gatewayUrl) )
			throw "Could not connected to gateway url "+gatewayUrl;
		return new AMFConnection({ cnx : c, error : function(e) throw e },[]);
		#end
	}

	public static function connect( nc ) {
		return new AMFConnection({ cnx : nc, error : function(e) throw e },[]);
	}

	#if flash9
	public static function registerClassAlias( s : String, cl : Class<Dynamic> ) {
		untyped __global__[ "flash.net.registerClassAlias" ]( s, cl );
	}
	#end

	#end

}
