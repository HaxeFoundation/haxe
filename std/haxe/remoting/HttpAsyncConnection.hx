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

class HttpAsyncConnection implements AsyncConnection {

	var __data : { url : String, error : Dynamic -> Void };
	var __path : Array<String>;

	function new(data,path) {
		__data = data;
		__path = path;
	}

	public function resolve( name ) : AsyncConnection {
		var c = new HttpAsyncConnection(__data,__path.copy());
		c.__path.push(name);
		return c;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) {
		var h = new haxe.Http(__data.url);
		#if (neko && no_remoting_shutdown)
			h.noShutdown = true;
		#end
		var s = new haxe.Serializer();
		s.serialize(__path);
		s.serialize(params);
		h.setHeader("X-Haxe-Remoting","1");
		h.setParameter("__x",s.toString());
		var error = __data.error;
		h.onData = function( response : String ) {
			var ok = true;
			var ret;
			try {
				if( response.substr(0,3) != "hxr" ) throw "Invalid response : '"+response+"'";
				var s = new haxe.Unserializer(response.substr(3));
				ret = s.unserialize();
			} catch( err : Dynamic ) {
				ret = null;
				ok = false;
				error(err);
			}
			if( ok && onResult != null ) onResult(ret);
		};
		h.onError = error;
		h.request(true);
	}

	public static function urlConnect( url : String ) {
		return new HttpAsyncConnection({ url : url, error : function(e) throw e },[]);
	}

}
