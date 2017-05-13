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

class FlashJsConnection #if flash implements AsyncConnection implements Dynamic<AsyncConnection> #end {

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

	static var connections = new haxe.ds.StringMap<FlashJsConnection>();

	static function escapeString( s : String ) {
		#if flash
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
		try flash.external.ExternalInterface.addCallback("flashJsRemotingCall",doCall) catch( e : Dynamic ) {};
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
			var fobj : Dynamic = untyped (untyped js.Browser.document)[__data.flash]; // FIXME(bruno): Why is this necessary?
			if( fobj == null ) fobj = js.Browser.document.getElementById(flashObj);
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
