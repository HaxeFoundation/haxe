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
package haxe;

class Connection implements Dynamic<Connection> {

	var __data : Dynamic;
	var __path : Array<String>;

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	function __resolve(field) {
		var s = new Connection(__data,__path.copy());
		s.__path.push(field);
		return s;
	}

	public function call( params : Array<Dynamic> ) : Dynamic {
	#if flash
		var p = __path.copy();
		var f = p.pop();
		var path = p.join(".");
		var s = new Serializer();
		s.serialize(params);
		var params = s.toString().split("\\").join("\\\\");
		var s = flash.external.ExternalInterface.call("haxe.Connection.doCall",path,f,params);
		if( s == null )
			throw "Failed to call JS method "+__path.join(".");
		return new Unserializer(s).unserialize();
	#else js
		var p = __path.copy();
		var f = p.pop();
		var path = p.join(".");
		var s = new Serializer();
		s.serialize(params);
		var params = s.toString();
		var s = __data.remotingCall(path,f,params);
		if( s == null )
			throw "Failed to call Flash method "+__path.join(".");
		return new Unserializer(s).unserialize();
	#else neko
		var cnx = AsyncConnection.urlConnect(__data);
		var result = null;
		untyped cnx.__path = __path;
		cnx.onError = function(err) { throw err; };
		cnx.call(params,function(d) { result = d; });
		return result;
	#else error
	#end
	}


	static function doCall( path : String, f : String, params : String ) : String {
		try {
			var params = new Unserializer(params).unserialize();
			#if flash
			var obj = flash.Lib.eval(path);
			#else js
			var obj = js.Lib.eval(path);
			#else true
			var obj = null;
			#end
			var fun = Reflect.field(obj,f);
			if( fun == null )
				throw "Invalid call : "+path+"."+f;
			var v = Reflect.callMethod(obj,fun,params);
			var s = new Serializer();
			s.serialize(v);
			#if flash
			return s.toString().split("\\").join("\\\\");
			#else js
			return s.toString();
			#else true
			return null;
			#end
		} catch( e : Dynamic ) {
			var s = new Serializer();
			s.serializeException(e);
			return s.toString();
		}
	}

	// ---- platform-specific ----

	#if flash

	static function __init__() {
		flash.external.ExternalInterface.addCallback("remotingCall",null,doCall);
	}

	public static function jsConnect() : Connection {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		if( flash.external.ExternalInterface.call("haxe.Connection.jsRemoting") != "yes" )
			throw "haxe.Connection is not available in JavaScript";
		return new Connection(null,[]);
	}

	#else js

	static function jsRemoting() {
		return "yes";
	}

	public static function flashConnect( objId : String ) : Connection {
		var x : Dynamic = untyped window.document[objId];
		if( x == null )
			throw "Could not find flash object '"+objId+"'";
		if( x.remotingCall == null ) throw "The flash object is not ready or does not contain haxe.Connection";
		return new Connection(x,[]);
	}

	#else neko

	public static function urlConnect( url : String ) : Connection {
		return new Connection(url,[]);
	}

	#end

}
