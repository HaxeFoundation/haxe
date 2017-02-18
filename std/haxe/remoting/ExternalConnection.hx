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

/**
	Synchronous communications between Flash and Javascript.
**/
@:expose
class ExternalConnection implements Connection implements Dynamic<Connection> {

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

	#if flash
	static function escapeString( s : String ) {
		return s.split("\\").join("\\\\");
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
			#if js_unflatten
				data = flash.external.ExternalInterface.call("haxe.remoting.ExternalConnection.doCall",__data.name,__path.join("."),params);
			#else
				data = flash.external.ExternalInterface.call("haxe_remoting_ExternalConnection.doCall",__data.name,__path.join("."),params);
			#end
		#elseif js
			var fobj : Dynamic = (untyped js.Browser.document)[cast __data.flash]; // FIXME(bruno): Why is this necessary?
			if( fobj == null ) fobj = js.Browser.document.getElementById(__data.flash);
			if( fobj == null ) throw "Could not find flash object '"+__data.flash+"'";
			try	data = fobj.externalRemotingCall(__data.name,__path.join("."),params) catch( e : Dynamic ) {};
		#end
		if( data == null ) {
			#if js
			var domain, pageDomain;
			try {
				// check that swf in on the same domain
				domain = fobj.src.split("/")[2];
				pageDomain = js.Browser.location.host;
			} catch( e : Dynamic ) {
				domain = null;
				pageDomain = null;
			}
			if( domain != pageDomain )
				throw "ExternalConnection call failure : SWF need allowDomain('"+pageDomain+"')";
			#end
			throw "Call failure : ExternalConnection is not " + #if flash "compiled in JS" #else "initialized in Flash" #end;
		}
		return new haxe.Unserializer(data).unserialize();
	}

	static var connections = new haxe.ds.StringMap<ExternalConnection>();

	@:keep
	static function doCall( name : String, path : String, params : String ) : String {
		try {
			var cnx = connections.get(name);
			if( cnx == null ) throw "Unknown connection : "+name;
			if( cnx.__data.ctx == null ) throw "No context shared for the connection "+name;
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
		#if as3
		return "";
		#end
	}

	#if flash

	public static function jsConnect( name : String, ?ctx : Context ) {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		try flash.external.ExternalInterface.addCallback("externalRemotingCall",doCall) catch( e : Dynamic ) {};
		var cnx = new ExternalConnection({ name : name, ctx : ctx },[]);
		connections.set(name,cnx);
		return cnx;
	}

	#elseif js

	public static function flashConnect( name : String, flashObjectID : String, ?ctx : Context ) {
		var cnx = new ExternalConnection({ ctx : ctx, name : name, flash : flashObjectID },[]);
		connections.set(name,cnx);
		return cnx;
	}

	#end

}
