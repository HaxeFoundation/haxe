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
		#if js
			h.async = false;
		#end
		#if (neko && no_remoting_shutdown)
			h.noShutdown = true;
		#end
		#if (neko || php || cpp)
			h.cnxTimeout = TIMEOUT;
		#end
		var files	= new List();
		function searchFile( o : Dynamic ) {
			if ( Std.is( o, Array ) ) {
				var a	: Array<Dynamic>	= cast o;
				for ( i in 0...a.length ) {
					var p	= a [ i ];
					if ( p == null )	continue;
					if ( p.param != null && p.filename != null && p.bytes != null  ) {
						files.add( cast p );
						a[ i ]	= '__file__${ p.param }';
					}else if ( Std.is( p, Array ) || Reflect.isObject( p ) ) {
						searchFile( p );
					}
				}
			}else if ( Reflect.isObject( o ) ) {
				for ( k in Reflect.fields( o ) ) {
					var p	= Reflect.getProperty( o, k );
					if ( p == null )	continue;
					if ( p.param != null && p.filename != null && p.bytes != null  ) {
						files.add( cast p );
						Reflect.setProperty( o, k ,'__file__${ p.param }' );
					}else if ( Std.is( p, Array ) || Reflect.isObject( p ) ) {
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
		h.onData = function(d) { data = d; };
		h.onError = function(e) { throw e; };
		h.request(true);
		if( data.substr(0,3) != "hxr" )
			throw "Invalid response : '"+data+"'";
		data = data.substr(3);
		return new haxe.Unserializer(data).unserialize();
	}

	#if (js || neko || php)

	public static function urlConnect( url : String ) {
		return new HttpConnection(url,[]);
	}

	#end

	#if neko
	public static function handleRequest( ctx : Context ) {
		var v	= null;	
		var mutlipartParams	= null;
		if ( neko.Web.getClientHeader( "X-Haxe-Remoting" ) != null ) {
			var ct	= neko.Web.getClientHeader( "Content-Type" );
			if ( ct != null && ct.indexOf( "multipart/form-data" ) != -1 ) {
				mutlipartParams	= neko.Web.getMultipartParams();
				v	= mutlipartParams.get( "__x" );
			}else
				v	= neko.Web.getParams().get( "__x" );
			if ( v != null ) {
				neko.Lib.print( processRequest( v, ctx, mutlipartParams ) );
				return true;
			}
		}
		
		return false;
	}
	#elseif php
	public static function handleRequest( ctx : Context ) {
		var v	= null;		
		var mutlipartParams	= null;
		if ( php.Web.getClientHeader( "X-Haxe-Remoting" ) != null ) {
			var ct	= php.Web.getClientHeader( "Content-Type" );
			if ( ct != null && ct.indexOf( "multipart/form-data" ) != -1 ) {
				mutlipartParams	= php.Web.getMultipartParams();
				v	= mutlipartParams.get( "__x" );
			}else
				v	= php.Web.getParams().get( "__x" );
			if ( v != null ) {
				php.Lib.print( processRequest( v, ctx ) );
				return true;
			}
		}
		return false;
	}
	#end
	
	#if neko
	public static function processRequest( requestData : String, ctx : Context, ?multipartParams : haxe.ds.StringMap<Dynamic> ) : String {
		try {
			var u = new haxe.Unserializer(requestData);
			var path = u.unserialize();
			var args : Array<Dynamic> = cast u.unserialize();
			
			if( multipartParams != null ){					
				function searchFile( o : Dynamic ) {
					if ( Std.is( o, Array ) ) {
						var a	: Array<Dynamic>	= cast o;
						for ( i in 0...a.length ) {
							var arg	= a [ i ];
							if ( arg	== null )	continue;
							if ( Std.is( arg, String ) && StringTools.startsWith( arg, "__file__" ) ) {
								var s	: String	= cast arg;
								a[ i ]	= multipartParams.get( s.substr( 8 ) );
							}else if ( Std.is( arg, Array ) || ( Reflect.isObject( arg ) && !Std.is( arg, String ) ) ) {
								searchFile( arg );
							}
						}
					}else if ( Reflect.isObject( o ) && !Std.is( o, String ) ) {
						for ( k in Reflect.fields( o ) ) {
							var arg	= Reflect.getProperty( o, k );
							if ( arg	== null )	continue;
							if ( Std.is( arg, String ) && StringTools.startsWith( arg, "__file__" ) ) {
								var s	: String	= cast arg;
								Reflect.setProperty( o, k , multipartParams.get( s.substr( 8 ) ) );
							}else if ( Std.is( arg, Array ) || ( Reflect.isObject( arg ) && !Std.is( arg, String ) ) ) {
								searchFile( arg );
							}
						}
					}
				}
				searchFile( args );
			}
			
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
	#elseif php
	public static function processRequest( requestData : String, ctx : Context, ?multipartParams : haxe.ds.StringMap<Dynamic> ) : String {
		try {
			var u = new haxe.Unserializer(requestData);
			var path = u.unserialize();
			var args : Array<Dynamic> = cast u.unserialize();
			
			if( multipartParams != null ){
				function searchFile( o : Dynamic ) {
					if ( Std.is( o, Array ) ) {
						var a	: Array<Dynamic>	= cast o;
						for ( i in 0...a.length ) {
							var arg	= a [ i ];
							if ( arg	== null )	continue;
							if ( Std.is( arg, String ) && StringTools.startsWith( arg, "__file__" ) ) {
								var s	: String	= cast arg;
								a[ i ]	= multipartParams.get( s.substr( 8 ) );
							}else if ( Std.is( arg, Array ) || ( Reflect.isObject( arg ) && !Std.is( arg, String ) ) ) {
								searchFile( arg );
							}
						}
					}else if ( Reflect.isObject( o ) && !Std.is( o, String ) ) {
						for ( k in Reflect.fields( o ) ) {
							var arg	= Reflect.getProperty( o, k );
							if ( arg	== null )	continue;
							if ( Std.is( arg, String ) && StringTools.startsWith( arg, "__file__" ) ) {
								var s	: String	= cast arg;
								Reflect.setProperty( o, k , multipartParams.get( s.substr( 8 ) ) );
							}else if ( Std.is( arg, Array ) || ( Reflect.isObject( arg ) && !Std.is( arg, String ) ) ) {
								searchFile( arg );
							}
						}
					}
				}
				searchFile( args );
			}
			
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
	#end

}
