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
    Allows a synchronous connection to the given URL which should link to a Haxe server application.
*/
class HttpConnection implements Connection implements Dynamic<Connection> {

	public static var TIMEOUT = 10.;

	var __url : String;
	var __path : Array<String>;

	function new(url,path) {
		__url = url;
		__path = path;
	}

	public function resolve( name ) : Connection {
		var c = new HttpConnection(__url,__path.copy());
		c.__path.push(name);
		return c;
	}

	public function call( params : Array<Dynamic> ) : Dynamic {
		var data = null;
		var h = new haxe.Http(__url);
		#if (js && !nodejs)
			h.async = false;
		#end
		#if (neko && no_remoting_shutdown)
			h.noShutdown = true;
		#end
		#if (neko || php || cpp)
			h.cnxTimeout = TIMEOUT;
		#end
		var s = new haxe.Serializer();
		s.serialize(__path);
		s.serialize(params);
		h.setHeader("X-Haxe-Remoting","1");
		h.setParameter("__x",s.toString());
		h.onData = function(d) { data = d; };
		h.onError = function(e) { throw e; };
		h.request(true);
		if( data.substr(0,3) != "hxr" )
			throw "Invalid response : '"+data+"'";
		data = data.substr(3);
		return new haxe.Unserializer(data).unserialize();
	}

	#if !flash

	public static function urlConnect( url : String ) {
		return new HttpConnection(url,[]);
	}

	#end

	#if neko
	public static function handleRequest( ctx : Context ) {
		var v = neko.Web.getParams().get("__x");
		if( neko.Web.getClientHeader("X-Haxe-Remoting") == null || v == null )
			return false;
		neko.Lib.print(processRequest(v,ctx));
		return true;
	}
	#elseif php
	public static function handleRequest( ctx : Context ) {
		var v = php.Web.getParams().get("__x");
		if( php.Web.getClientHeader("X-Haxe-Remoting") == null || v == null )
			return false;
		php.Lib.print(processRequest(v,ctx));
		return true;
	}
	#end

	public static function processRequest( requestData : String, ctx : Context ) : String {
		try {
			var u = new haxe.Unserializer(requestData);
			var path = u.unserialize();
			var args = u.unserialize();
			var data = ctx.call(path,args);
			var s = new haxe.Serializer();
			s.serialize(data);
			return "hxr" + s.toString();
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return "hxr" + s.toString();
		}
	}

}
