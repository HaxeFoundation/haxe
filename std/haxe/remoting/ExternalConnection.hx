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

/**
	Synchronous communications between Flash and Javascript.
**/
class ExternalConnection implements Connection, implements Dynamic<Connection> {

	var __data : { name : String, ctx : Context, #if js flash : String #end };
	var __path : Array<String>;

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	public function resolve(field) : Connection {
		var e = new ExternalConnection(__data,__path.copy());
		e.__path.push(field);
		return e;
	}

	public function close() {
		connections.remove(__data.name);
	}

	#if flash9
	static function escapeString( s : String ) {
		return s.split("\\").join("\\\\");
	}
	#elseif flash
	static function escapeString( s : String ) {
		return s.split("\\").join("\\\\").split("&").join("&amp;");
	}
	#else
	static inline function escapeString(s) {
		return s;
	}
	#end

	public function call( params : Array<Dynamic> ) : Dynamic {
		var s = new haxe.Serializer();
		s.serialize(params);
		var params = escapeString(s.toString());
		var data = null;
		#if flash
			data = flash.external.ExternalInterface.call("haxe.remoting.ExternalConnection.doCall",__data.name,__path.join("."),params);
		#elseif js
			var fobj : Dynamic = untyped window.document[__data.flash];
			if( fobj == null ) fobj = untyped window.document.getElementById[__data.flash];
			if( fobj == null ) throw "Could not find flash object '"+__data.flash+"'";
			try	data = fobj.externalRemotingCall(__data.name,__path.join("."),params) catch( e : Dynamic ) {};
		#end
		if( data == null )
			throw "Call failure : ExternalConnection is not " + #if flash "compiled in JS" #else "initialized in Flash" #end;
		return new haxe.Unserializer(data).unserialize();
	}

	static var connections = new Hash<ExternalConnection>();

	static function doCall( name : String, path : String, params : String ) : String {
		try {
			var cnx = connections.get(name);
			if( cnx == null ) throw "Unknown connection : "+name;
			var params = new haxe.Unserializer(params).unserialize();
			var ret = cnx.__data.ctx.call(path.split("."),params);
			var s = new haxe.Serializer();
			s.serialize(ret);
			#if flash
			return escapeString(s.toString());
			#else
			return s.toString()+"#";
			#end
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return s.toString();
		}
		#if as3gen
		return "";
		#end
	}

	#if flash

	public static function jsConnect( name : String, ctx : Context ) {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		#if flash9
		try flash.external.ExternalInterface.addCallback("externalRemotingCall",doCall) catch( e : Dynamic ) {};
		#else
		flash.external.ExternalInterface.addCallback("externalRemotingCall",null,doCall);
		#end
		var cnx = new ExternalConnection({ name : name, ctx : ctx },[]);
		connections.set(name,cnx);
		return cnx;
	}

	#elseif js

	public static function flashConnect( name : String, flashObjectID : String, ctx : Context ) {
		var cnx = new ExternalConnection({ ctx : ctx, name : name, flash : flashObjectID },[]);
		connections.set(name,cnx);
		return cnx;
	}

	#end

}
