/*
 * Copyright (C)2005-2012 Haxe Foundation
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

class HttpAsyncConnection implements AsyncConnection implements Dynamic<AsyncConnection> {

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
		
		var files	= new List();
		function searchFile( o : Dynamic ) {
			if ( Std.is( o, Array ) ) {
				var a	: Array<Dynamic>	= cast o;
				for ( i in 0...a.length ) {
					var p	= a [ i ];
					if ( p != null && p.param != null && p.filename != null && p.bytes != null  ) {
						files.add( cast p );
						a[ i ]	= '__file__${ p.param }';
					}else if ( Std.is( p, Array ) || ( Reflect.isObject( p ) && !Std.is( p, String ) ) ) {
						searchFile( p );
					}
				}
			}else if ( Reflect.isObject( o ) && !Std.is( o, String ) ) {
				for ( k in Reflect.fields( o ) ) {
					var p	= Reflect.getProperty( o, k );
					if ( p != null && p.param != null && p.filename != null && p.bytes != null  ) {
						files.add( cast p );
						Reflect.setProperty( o, k ,'__file__${ p.param }' );
					}else if ( Std.is( p, Array ) || ( Reflect.isObject( p ) && !Std.is( p, String ) ) ) {
						searchFile( p );
					}
				}
			}
		}
		searchFile( params );
		var s = new haxe.Serializer();
		s.serialize(__path);
		s.serialize(params);
		h.setHeader("X-Haxe-Remoting","1");
		h.setParameter("__x", s.toString());
		for ( file in files )	h.addFileTransfer( file.param, file.filename, file.bytes, file.mimeType );
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
