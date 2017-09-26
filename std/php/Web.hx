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
package php;

import haxe.io.Bytes;
import php.Syntax.*;
import php.Global.*;
import php.SuperGlobal.*;

/**
	This class is used for accessing the local Web server and the current
	client request and information.
**/
class Web {

	/**
		Returns the GET and POST parameters.
	**/
	public static function getParams() : Map<String,String> {
		#if force_std_separator
		var h = Lib.hashOfAssociativeArray(_POST);
		var params = getParamsString();
		if( params == "" )
			return h;
		for( p in ~/[;&]/g.split(params) ) {
			var a = p.split("=");
			var n = a.shift();
			h.set(StringTools.urlDecode(n),StringTools.urlDecode(a.join("=")));
		}
		return h;
		#else
		return Lib.hashOfAssociativeArray(array_merge(_GET, _POST));
		#end
	}

	/**
		Returns an Array of Strings built using GET / POST values.
		If you have in your URL the parameters `a[]=foo;a[]=hello;a[5]=bar;a[3]=baz` then
		`php.Web.getParamValues("a")` will return `["foo","hello",null,"baz",null,"bar"]`.
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

		if (res.length == 0) {
			var post:haxe.ds.StringMap<Dynamic> = Lib.hashOfAssociativeArray(_POST);
			var data = post.get(param);
			if (is_array(data)) {
				foreach(data, function(key:Int, value:String) {
					res[key] = value;
				});
			}
		}

		if (res.length == 0)
			return null;
		return res;
	}

	/**
		Returns the local server host name.
	**/
	public static inline function getHostName() : String {
		return _SERVER['SERVER_NAME'];
	}

	/**
		Surprisingly returns the client IP address.
	**/
	public static inline function getClientIP() : String {
		return _SERVER['REMOTE_ADDR'];
	}

	/**
		Returns the original request URL (before any server internal redirections).
	**/
	public static function getURI() : String {
		var s : String = _SERVER['REQUEST_URI'];
		return s.split("?")[0];
	}

	/**
		Tell the client to redirect to the given url ("Location" header).
	**/
	public static function redirect( url : String ) {
		header("Location: " + url);
	}

	/**
		Set an output header value. If some data have been printed, the headers have
		already been sent so this will raise an exception.
	**/
	public static inline function setHeader( h : String, v : String ) {
		header('$h: $v');
	}

	/**
		Set the HTTP return code. Same remark as `php.Web.setHeader()`.
		See status code explanation here: http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
	**/
	public static function setReturnCode( r : Int ) {
		var code : String;
		switch(r) {
			case 100: code = "100 Continue";
			case 101: code = "101 Switching Protocols";
			case 200: code = "200 OK";
			case 201: code = "201 Created";
			case 202: code = "202 Accepted";
			case 203: code = "203 Non-Authoritative Information";
			case 204: code = "204 No Content";
			case 205: code = "205 Reset Content";
			case 206: code = "206 Partial Content";
			case 300: code = "300 Multiple Choices";
			case 301: code = "301 Moved Permanently";
			case 302: code = "302 Found";
			case 303: code = "303 See Other";
			case 304: code = "304 Not Modified";
			case 305: code = "305 Use Proxy";
			case 307: code = "307 Temporary Redirect";
			case 400: code = "400 Bad Request";
			case 401: code = "401 Unauthorized";
			case 402: code = "402 Payment Required";
			case 403: code = "403 Forbidden";
			case 404: code = "404 Not Found";
			case 405: code = "405 Method Not Allowed";
			case 406: code = "406 Not Acceptable";
			case 407: code = "407 Proxy Authentication Required";
			case 408: code = "408 Request Timeout";
			case 409: code = "409 Conflict";
			case 410: code = "410 Gone";
			case 411: code = "411 Length Required";
			case 412: code = "412 Precondition Failed";
			case 413: code = "413 Request Entity Too Large";
			case 414: code = "414 Request-URI Too Long";
			case 415: code = "415 Unsupported Media Type";
			case 416: code = "416 Requested Range Not Satisfiable";
			case 417: code = "417 Expectation Failed";
			case 500: code = "500 Internal Server Error";
			case 501: code = "501 Not Implemented";
			case 502: code = "502 Bad Gateway";
			case 503: code = "503 Service Unavailable";
			case 504: code = "504 Gateway Timeout";
			case 505: code = "505 HTTP Version Not Supported";
			default: code = Std.string(r);
		}
		header("HTTP/1.1 " + code, true, r);
	}

	/**
		Retrieve a client header value sent with the request.
	**/
	public static function getClientHeader( k : String ) : String {
		return loadClientHeaders().get(k);
	}


	private static var _clientHeaders : Map<String,String>;
	/**
		Based on https://github.com/ralouphie/getallheaders
	**/
	static function loadClientHeaders():Map<String,String> {
		if(_clientHeaders != null) return _clientHeaders;

		_clientHeaders = new Map();

		if(function_exists('getallheaders')) {
			foreach(getallheaders(), function(key:String, value:Dynamic) {
				_clientHeaders.set(key, Std.string(value));
			});
			return _clientHeaders;
		}

		var copyServer = [
			'CONTENT_TYPE'   => 'Content-Type',
			'CONTENT_LENGTH' => 'Content-Length',
			'CONTENT_MD5'    => 'Content-Md5'
		];
		foreach(_SERVER, function(key:String, value:Dynamic) {
			if((substr(key, 0, 5):String) == 'HTTP_') {
				key = substr(key, 5);
				if(!copyServer.exists(key) || !isset(_SERVER[key])) {
					key = str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', key))));
					_clientHeaders[key] = Std.string(value);
				}
			} else if(copyServer[key] != null) {
				_clientHeaders[copyServer[key]] = Std.string(value);
			}
		});
		if(!_clientHeaders.exists('Authorization')) {
			if(isset(_SERVER['REDIRECT_HTTP_AUTHORIZATION'])) {
				_clientHeaders['Authorization'] = Std.string(_SERVER['REDIRECT_HTTP_AUTHORIZATION']);
			} else if(isset(_SERVER['PHP_AUTH_USER'])) {
				var basic_pass = isset(_SERVER['PHP_AUTH_PW']) ? Std.string(_SERVER['PHP_AUTH_PW']) : '';
				_clientHeaders['Authorization'] = 'Basic ' + base64_encode(_SERVER['PHP_AUTH_USER'] + ':' + basic_pass);
			} else if(isset(_SERVER['PHP_AUTH_DIGEST'])) {
				_clientHeaders['Authorization'] = Std.string(_SERVER['PHP_AUTH_DIGEST']);
			}
		}

		return _clientHeaders;
	}

	/**
		Retrieve all the client headers.
	**/
	public static function getClientHeaders():List<{value:String, header:String}> {
		var headers = loadClientHeaders();
		var result = new List();
		for(key in headers.keys()) {
			result.push({value:headers.get(key), header:key});
		}
		return result;
	}

	/**
		Returns all the GET parameters `String`
	**/
	public static function getParamsString() : String {
		if(isset(_SERVER['QUERY_STRING']))
			return _SERVER['QUERY_STRING'];
		else
			return "";
	}

	/**
		Returns all the POST data. POST Data is always parsed as
		being application/x-www-form-urlencoded and is stored into
		the getParams hashtable. POST Data is maximimized to 256K
		unless the content type is multipart/form-data. In that
		case, you will have to use `php.Web.getMultipart()` or
		`php.Web.parseMultipart()` methods.
	**/
	public static function getPostData() : Null<String> {
		var h = fopen("php://input", "r");
		var bsize = 8192;
		var max = 32;
		var data : String = null;
		var counter = 0;
		while (!feof(h) && counter < max) {
			data = Syntax.binop(data, ' . ', fread(h, bsize));
			counter++;
		}
		fclose(h);
		return data;
	}

	/**
		Returns an hashtable of all Cookies sent by the client.
		Modifying the hashtable will not modify the cookie, use `php.Web.setCookie()`
		instead.
	**/
	public static function getCookies():Map<String,String> {
		return Lib.hashOfAssociativeArray(_COOKIE);
	}


	/**
		Set a Cookie value in the HTTP headers. Same remark as `php.Web.setHeader()`.
	**/
	public static function setCookie( key : String, value : String, ?expire: Date, ?domain: String, ?path: String, ?secure: Bool, ?httpOnly: Bool ) {
		var t = expire == null ? 0 : Std.int(expire.getTime()/1000.0);
		if(path == null) path = '/';
		if(domain == null) domain = '';
		if(secure == null) secure = false;
		if(httpOnly == null) httpOnly = false;
		setcookie(key, value, t, path, domain, secure, httpOnly);
	}

	/**
		Returns an object with the authorization sent by the client (Basic scheme only).
	**/
	public static function getAuthorization() : { user : String, pass : String } {
		if(!isset(_SERVER['PHP_AUTH_USER']))
			return null;
		return {user: _SERVER['PHP_AUTH_USER'], pass: _SERVER['PHP_AUTH_PW']};
	}

	/**
		Get the current script directory in the local filesystem.
	**/
	public static inline function getCwd() : String {
		return dirname(_SERVER['SCRIPT_FILENAME']) + "/";
	}

	/**
		Get the multipart parameters as an hashtable. The data
		cannot exceed the maximum size specified.
	**/
	public static function getMultipart( maxSize : Int ) : Map<String,String> {
		var h = new haxe.ds.StringMap();
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
		}, function(str,pos,len) {
			maxSize -= len;
			if( maxSize < 0 )
				throw "Maximum size reached";
			buf.addSub(str.toString(),pos,len);
		});
		if( curname != null )
			h.set(curname,buf.toString());
		return h;
	}

	/**
		Parse the multipart data. Call `onPart` when a new part is found
		with the part name and the filename if present
		and `onData` when some part data is readed. You can this way
		directly save the data on hard drive in the case of a file upload.
	**/
	public static function parseMultipart( onPart : String -> String -> Void, onData : Bytes -> Int -> Int -> Void ) : Void {
		var post = Lib.hashOfAssociativeArray(_POST);

		for (key in post.keys())
		{
			onPart(key, "");
			var v = post.get(key);
			onData(Bytes.ofString(v), 0, strlen(v));
		}

		if(!isset(_FILES)) return;
		var parts : Array<String> = @:privateAccess Array.wrap(array_keys(_FILES));
		for(part in parts) {
			var info : NativeAssocArray<Dynamic> = _FILES[part];
			var tmp : String = info['tmp_name'];
			var file : String = info['name'];
			var err : Int = info['error'];

			if(err > 0) {
				switch(err) {
					case 1: throw "The uploaded file exceeds the max size of " + ini_get('upload_max_filesize');
					case 2: throw "The uploaded file exceeds the max file size directive specified in the HTML form (max is" + ini_get('post_max_size') + ")";
					case 3: throw "The uploaded file was only partially uploaded";
					case 4: continue; // No file was uploaded
					case 6: throw "Missing a temporary folder";
					case 7: throw "Failed to write file to disk";
					case 8: throw "File upload stopped by extension";
				}
			}
			onPart(part, file);
			if ("" != file)
			{
				var h = fopen(tmp, "r");
				var bsize = 8192;
				while (!feof(h)) {
					var buf : String = fread(h, bsize);
					var size : Int = strlen(buf);
					onData(Bytes.ofString(buf), 0, size);
				}
				fclose(h);
			}
		}
	}

	/**
		Flush the data sent to the client. By default on Apache, outgoing data is buffered so
		this can be useful for displaying some long operation progress.
	**/
	public static inline function flush() : Void {
		flush();
	}

	/**
		Get the HTTP method used by the client.
	**/
	public static function getMethod() : String {
		if(isset(_SERVER['REQUEST_METHOD']))
			return _SERVER['REQUEST_METHOD'];
		else
			return null;
	}

	public static var isModNeko(default,null) : Bool;

	static function __init__() {
		isModNeko = !Lib.isCli();
	}
}
