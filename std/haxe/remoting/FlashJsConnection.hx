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

class FlashJsConnection #if flash implements AsyncConnection, implements Dynamic<AsyncConnection> #end {

#if flash

	var __path : Array<String>;
	var __data : {
		id : String,
		name : String,
		ctx : Context,
		error : Dynamic -> Void,
		timer : haxe.Timer,
		queue : Array<Void -> Void>,
	};

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	public function close() {
		connections.remove(__data.name);
	}

	public function resolve( name ) : AsyncConnection {
		var c = new FlashJsConnection(__data,__path.copy());
		c.__path.push(name);
		return c;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) {
		var s = new haxe.Serializer();
		s.serialize(params);
		var params = escapeString(s.toString());
		var error = __data.error;
		__data.queue.push(function() {
			var data = flash.external.ExternalInterface.call("haxe.remoting.FlashJsConnection.flashCall",__data.id,__data.name,__path.join("."),params);
			var v : Dynamic;
			try {
				if( data == null )
					throw "Call failure : FlashJsConnection is not compiled in JS";
				v = new haxe.Unserializer(data).unserialize();
			} catch( e : Dynamic ) {
				error(e);
				return;
			}
			if( onResult != null )
				onResult(v);
		});
		if( __data.timer == null ) {
			__data.timer = new haxe.Timer(1);
			__data.timer.run = function() {
				var q = __data.queue.shift();
				if( q == null ) {
					__data.timer.stop();
					__data.timer = null;
					return;
				}
				q();
			};
		}
	}

	static var connections = new Hash<FlashJsConnection>();

	static function escapeString( s : String ) {
		#if flash9
		return s.split("\\").join("\\\\");
		#else
		return s.split("\\").join("\\\\").split("&").join("&amp;");
		#end
	}

	static function doCall( name : String, path : String, params : String ) : String {
		try {
			var cnx = connections.get(name);
			if( cnx == null ) throw "Unknown connection : "+name;
			if( cnx.__data.ctx == null ) throw "No context shared for the connection "+name;
			var params = new haxe.Unserializer(params).unserialize();
			var ret = cnx.__data.ctx.call(path.split("."),params);
			var s = new haxe.Serializer();
			s.serialize(ret);
			return escapeString(s.toString());
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return s.toString();
		}
		#if as3
		return "";
		#end
	}

	public static function connect( name : String, objId : String, ?ctx : Context ) {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		#if flash9
		try flash.external.ExternalInterface.addCallback("flashJsRemotingCall",doCall) catch( e : Dynamic ) {};
		#else
		flash.external.ExternalInterface.addCallback("flashJsRemotingCall",null,doCall);
		#end
		var cnx = new FlashJsConnection({
			id : objId,
			name : name,
			ctx : ctx,
			error : function(e) throw e,
			queue : [],
			timer : null,
		},[]);
		connections.set(name,cnx);
		return cnx;
	}

#elseif js

	static function flashCall( flashObj : String, name : String, path : String, params : String ) : String {
		try {
			var fobj : Dynamic = untyped window.document[flashObj];
			if( fobj == null ) fobj = untyped window.document.getElementById[flashObj];
			if( fobj == null ) throw "Could not find flash object '"+flashObj+"'";
			var data = null;
			try data = fobj.flashJsRemotingCall(name,path,params) catch( e : Dynamic ) {};
			if( data == null ) throw "Flash object "+flashObj+" does not have an active FlashJsConnection";
			return data;
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return s.toString();
		}
	}

#end

}
