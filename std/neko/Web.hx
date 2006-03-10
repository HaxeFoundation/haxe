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
	public static function params() {
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
		return new String(_get_client_header(untyped k.__s));
	}

	/**
		Returns all the GET parameters String
	**/
	public static function getParamsString() {
		return new String(_get_params_string());
	}

	/**
		Returns all the POST data (in case of file transfert for example).
	**/
	public static function getPostData() {
		return new String(_get_post_data());
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
		Get the current script directory in the local filesystem.
	**/
	public static function getCwd() {
		return new String(_get_cwd());
	}

	/**
		Set the main entry point function used to handle requests.
		Setting it back to null will disable code caching.
	**/
	public static function setMain( f : Void -> Void ) {
		_set_main(f);
	}

	private static var _set_main = Lib.load("mod_neko","cgi_set_main",1);
	private static var _get_host_name = Lib.load("mod_neko","get_host_name",0);
	private static var _get_client_ip = Lib.load("mod_neko","get_client_ip",0);
	private static var _get_uri = Lib.load("mod_neko","get_uri",0);
	private static var _cgi_redirect = Lib.load("mod_neko","redirect",1);
	private static var _cgi_set_header = Lib.load("mod_neko","set_header",2);
	private static var _set_return_code = Lib.load("mod_neko","set_return_code",1);
	private static var _get_client_header = Lib.load("mod_neko","get_client_header",1);
	private static var _get_params_string = Lib.load("mod_neko","get_params_string",0);
	private static var _get_post_data = Lib.load("mod_neko","get_post_data",0);
	private static var _get_params = Lib.load("mod_neko","get_params",0);
	private static var _get_cookies = Lib.load("mod_neko","get_cookies",0);
	private static var _set_cookie = Lib.load("mod_neko","set_cookie",2);
	private static var _get_cwd = Lib.load("mod_neko","cgi_get_cwd",0);

}
