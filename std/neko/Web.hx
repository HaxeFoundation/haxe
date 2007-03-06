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
package neko;

/**
	This class is used for accessing the local Web server and the current
	client request and informations.
**/
class Web {

	/**
		Returns the GET and POST parameters.
	**/
	public static function getParams() {
		var p = _get_params();
		var h = new Hash<String>();
		var k = "";
		while( p != null ) {
			untyped k.__s = p[0];
			h.set(k,new String(p[1]));
			p = untyped p[2];
		}
		return h;
	}

	/**
		Returns an Array of Strings built using GET / POST values.
		If you have in your URL the parameters [a[]=foo;a[]=hello;a[]=bar;a[]=baz] then
		[neko.Web.getParamValues("a")] will return [["foo","hello",null,"baz",null,"bar"]]
	**/
	public static function getParamValues( param : String ) : Array<String> {
		var reg = new EReg("^"+param+"(\\[|%5B)([0-9]*?)(\\]|%5D)=(.*?)$", "");
		var res = new Array<String>();
		var explore = function(data:String){
			if (data == null || data.length == 0)
				return;
			for (part in data.split("&")){
				if (reg.match(part)){
					var idx = reg.matched(2);
					var val = StringTools.urlDecode(reg.matched(4));
					if (idx == "")
						res.push(val);
					else
						res[Std.parseInt(idx)] = val;
				}
			}
		}
		explore(StringTools.replace(getParamsString(), ";", "&"));
		explore(getPostData());
		if (res.length == 0)
			return null;
		return res;
	}

	/**
		Returns the local server host name
	**/
	public static function getHostName() {
		return new String(_get_host_name());
	}

	/**
		Surprisingly returns the client IP address.
	**/
	public static function getClientIP() {
		return new String(_get_client_ip());
	}

	/**
		Returns the original request URL (before any server internal redirections)
	**/
	public static function getURI() {
		return new String(_get_uri());
	}

	/**
		Tell the client to redirect to the given url ("Location" header)
	**/
	public static function redirect( url : String ) {
		_cgi_redirect(untyped url.__s);
	}

	/**
		Set an output header value. If some data have been printed, the headers have
		already been sent so this will raise an exception.
	**/
	public static function setHeader( h : String, v : String ) {
		_cgi_set_header(untyped h.__s,untyped v.__s);
	}

	/**
		Set the HTTP return code. Same remark as setHeader.
	**/
	public static function setReturnCode( r : Int ) {
		_set_return_code(r);
	}

	/**
		Retrieve a client header value sent with the request.
	**/
	public static function getClientHeader( k : String ) {
		var v = _get_client_header(untyped k.__s);
		if( v == null )
			return null;
		return new String(v);
	}

	/**
		Retrieve all the client headers.
	**/
	public static function getClientHeaders() {
		var v = _get_client_headers();
		var a = new List();
		while( v != null ) {
			a.add({ header : new String(v[0]), value : new String(v[1]) });
			v = cast v[2];
		}
		return a;
	}

	/**
		Returns all the GET parameters String
	**/
	public static function getParamsString() {
		return new String(_get_params_string());
	}

	/**
		Returns all the POST data. POST Data is always parsed as
		being application/x-www-form-urlencoded and is stored into
		the getParams hashtable. POST Data is maximimized to 256K
		unless the content type is multipart/form-data. In that
		case, you will have to use [getMultipart] or [parseMultipart]
		methods.
	**/
	public static function getPostData() {
		var v = _get_post_data();
		if( v == null )
			return null;
		return new String(v);
	}

	/**
		Returns an hashtable of all Cookies sent by the client.
		Modifying the hashtable will not modify the cookie, use setCookie instead.
	**/
	public static function getCookies() {
		var p = _get_cookies();
		var h = new Hash<String>();
		var k = "";
		while( p != null ) {
			untyped k.__s = p[0];
			h.set(k,new String(p[1]));
			p = untyped p[2];
		}
		return h;
	}


	/**
		Set a Cookie value in the HTTP headers. Same remark as setClientHeader.
	**/
	public static function setCookie( k : String, v : String ) {
		_set_cookie(untyped k.__s,untyped v.__s);
	}

	/**
		Returns an object with the authorization sent by the client (Basic scheme only).
	**/
	public static function getAuthorization() : { user : String, pass : String } {
		var h = getClientHeader("Authorization");
		var reg = ~/^Basic ([^=]+)=*$/;
		if( h != null && reg.match(h) ){
			var val = reg.matched(1);
			untyped val = new String(_base_decode(val.__s,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".__s));
			var a = val.split(":");
			if( a.length != 2 ){
				throw "Unable to decode authorization.";
			}
			return {user: a[0],pass: a[1]};
		}
		return null;
	}

	/**
		Get the current script directory in the local filesystem.
	**/
	public static function getCwd() {
		return new String(_get_cwd());
	}

	/**
		Set the main entry point function used to handle requests.
		Setting it back to null will disable code caching.
	**/
	public static function cacheModule( f : Void -> Void ) {
		_set_main(f);
	}

	/**
		Get the multipart parameters as an hashtable. The data
		cannot exceed the maximum size specified.
	**/
	public static function getMultipart( maxSize : Int ) : Hash<String> {
		var h = new Hash();
		var buf : StringBuf = null;
		var curname = null;
		parseMultipart(function(p,_) {
			if( curname != null )
				h.set(curname,buf.toString());
			curname = p;
			buf = new StringBuf();
			maxSize -= p.length;
			if( maxSize < 0 )
				throw "Maximum size reached";
		},function(str,pos,len) {
			maxSize -= len;
			if( maxSize < 0 )
				throw "Maximum size reached";
			buf.addSub(str,pos,len);
		});
		if( curname != null )
			h.set(curname,buf.toString());
		return h;
	}

	/**
		Parse the multipart data. Call [onPart] when a new part is found
		with the part name and the filename if present
		and [onData] when some part data is readed. You can this way
		directly save the data on hard drive in the case of a file upload.
	**/
	public static function parseMultipart( onPart : String -> String -> Void, onData : String -> Int -> Int -> Void ) : Void {
		_parse_multipart(
			function(p,f) { onPart(new String(p),if( f == null ) null else new String(f)); },
			function(buf,pos,len) { onData(new String(buf),pos,len); }
		);
	}

	/**
		Flush the data sent to the client. By default on Apache, outgoing data is buffered so
		this can be useful for displaying some long operation progress.
	**/
	public static function flush() : Void {
		_flush();
	}

	public static var isModNeko(default,null) : Bool;

	static var _set_main : Dynamic;
	static var _get_host_name : Dynamic;
	static var _get_client_ip : Dynamic;
	static var _get_uri : Dynamic;
	static var _cgi_redirect : Dynamic;
	static var _cgi_set_header : Dynamic;
	static var _set_return_code : Dynamic;
	static var _get_client_header : Dynamic;
	static var _get_params_string : Dynamic;
	static var _get_post_data : Dynamic;
	static var _get_params : Dynamic;
	static var _get_cookies : Dynamic;
	static var _set_cookie : Dynamic;
	static var _get_cwd : Dynamic;
	static var _parse_multipart : Dynamic;
	static var _flush : Dynamic;
	static var _get_client_headers : Dynamic;
	static var _base_decode = Lib.load("std","base_decode",2);

	static function __init__() {
		var get_env = Lib.load("std","get_env",1);
		var ver = untyped get_env("MOD_NEKO".__s);
		untyped isModNeko = (ver != null);
		if( isModNeko ) {
			var lib = "mod_neko"+if( ver == untyped "1".__s ) "" else ver;
			_set_main = Lib.load(lib,"cgi_set_main",1);
			_get_host_name = Lib.load(lib,"get_host_name",0);
			_get_client_ip = Lib.load(lib,"get_client_ip",0);
			_get_uri = Lib.load(lib,"get_uri",0);
			_cgi_redirect = Lib.load(lib,"redirect",1);
			_cgi_set_header = Lib.load(lib,"set_header",2);
			_set_return_code = Lib.load(lib,"set_return_code",1);
			_get_client_header = Lib.load(lib,"get_client_header",1);
			_get_params_string = Lib.load(lib,"get_params_string",0);
			_get_post_data = Lib.load(lib,"get_post_data",0);
			_get_params = Lib.load(lib,"get_params",0);
			_get_cookies = Lib.load(lib,"get_cookies",0);
			_set_cookie = Lib.load(lib,"set_cookie",2);
			_get_cwd = Lib.load(lib,"cgi_get_cwd",0);
			_parse_multipart = try Lib.load(lib,"parse_multipart_data",2) catch( e : Dynamic ) function(a,b) { throw "Please upgrade Neko"; };
			_flush = try Lib.load(lib,"cgi_flush",0) catch( e : Dynamic ) function() { throw "Please upgrade Neko"; };
			_get_client_headers = try Lib.load(lib,"get_client_headers",0) catch( e : Dynamic ) function() { throw "Please upgrade Neko"; };
		} else {
			var a0 = untyped __dollar__loader.args[0];
			if( a0 != null ) a0 = new String(a0);
			_set_main = function(f) { };
			_get_host_name = function() { return untyped "localhost".__s; };
			_get_client_ip = function() { return untyped "127.0.0.1".__s; };
			_get_uri = function() {
				return untyped (if( a0 == null ) "/" else "/?"+a0).__s;
			};
			_cgi_redirect = function(v) { Lib.print("Location: "+v+"\n"); };
			_cgi_set_header = function(h,v) { };
			_set_return_code = function(i) { };
			_get_client_header = function(h) { return null; };
			_get_client_headers = function() { return null; };
			_get_params_string = function() {
				return untyped (if( a0 == null ) "" else a0).__s;
			};
			_get_post_data = function() { return null; };
			_get_params = function() {
				var l = null;
				if( a0 == null )
					return null;
				for( p in a0.split(";") ) {
					var k = p.split("=");
					if( k.length == 2 )
						l = untyped [k[0].__s,k[1].__s,l];
				}
				return l;
			};
			_get_cookies = function() { return null; }
			_set_cookie = function(k,v) { };
			_get_cwd = Lib.load("std","get_cwd",0);
			_parse_multipart = function(a,b) { throw "Not supported"; };
			_flush = function() { };
		}
	}

}
