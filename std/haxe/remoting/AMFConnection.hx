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
	Allows a connection to an AMF Remoting server such as Flash Media Server or AMFPHP.
*/
class AMFConnection implements AsyncConnection implements Dynamic<AsyncConnection> {

	var __data : {
		error : Dynamic -> Void,
		#if flash
		cnx : flash.net.NetConnection,
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
		#if flash
		p.unshift(new flash.net.Responder(onResult,__data.error));
		#else
		p.unshift({ onStatus : __data.error, onResult : onResult });
		#end
		p.unshift(__path.join("."));
		untyped __data.cnx.call.apply(__data,p);
	}

	#if flash
	public static function urlConnect( gatewayUrl : String ) {
		var c = new flash.net.NetConnection();
		var cnx = new AMFConnection({ cnx : c, error : function(e) throw e },[]);
		c.addEventListener(flash.events.NetStatusEvent.NET_STATUS,function(e:flash.events.NetStatusEvent) {
			cnx.__data.error(e);
		});
		c.connect(gatewayUrl);
		return cnx;
	}

	public static function connect( nc ) {
		return new AMFConnection({ cnx : nc, error : function(e) throw e },[]);
	}

	public static function registerClassAlias( s : String, cl : Class<Dynamic> ) {
		untyped __global__[ "flash.net.registerClassAlias" ]( s, cl );
	}
	#end

}
