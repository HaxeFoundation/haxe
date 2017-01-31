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
package haxe;

#if sys

import sys.net.Host;
import sys.net.Socket;

#end

/**
	This class can be used to handle Http requests consistently across
	platforms. There are two intended usages:

	- call `haxe.Http.requestUrl(url)` and receive the result as a `String`
	(not available on flash)
	- create a `new haxe.Http(url)`, register your callbacks for `onData`, 
	`onError` and `onStatus`, then call `request()`.
**/
class Http {

	/**
		The url of `this` request. It is used only by the `request()` method and
		can be changed in order to send the same request to different target
		Urls.
	**/
	public var url : String;
	public var responseData(default, null) : Null<String>;
#if sys
	public var noShutdown : Bool;
	public var cnxTimeout : Float;
	public var responseHeaders : Map<String,String>;
	var chunk_size : Null<Int>;
	var chunk_buf : haxe.io.Bytes;
	var file : { param : String, filename : String, io : haxe.io.Input, size : Int, mimeType : String };
#elseif (js && !nodejs)
	public var async : Bool;
	public var withCredentials : Bool;
#end
	var postData : String;
	var headers : List<{ header:String, value:String }>;
	var params : List<{ param:String, value:String }>;

	#if sys
	public static var PROXY : { host : String, port : Int, auth : { user : String, pass : String } } = null;
	#end

	/**
		Creates a new Http instance with `url` as parameter.

		This does not do a request until `request()` is called.

		If `url` is null, the field url must be set to a value before making the
		call to `request()`, or the result is unspecified.

		(Php) Https (SSL) connections are allowed only if the OpenSSL extension
		is enabled.
	**/
	public function new( url : String ) {
		this.url = url;
		headers = new List<{ header:String, value:String }>();
		params = new List<{ param:String, value:String }>();

		#if (js && !nodejs)
		async = true;
		withCredentials = false;
		#elseif sys
		cnxTimeout = 10;
		#end
		#if php
		noShutdown = ! untyped __call__('function_exists', 'stream_socket_shutdown');
		#end
	}

	/**
		Sets the header identified as `header` to value `value`.

		If `header` or `value` are null, the result is unspecified.

		This method provides a fluent interface.
	**/
	public function setHeader( header : String, value : String ):Http {
		headers = Lambda.filter(headers, function(h) return h.header != header);
		headers.push({ header:header, value:value });
		return this;
	}

	public function addHeader( header : String, value : String ):Http {
		headers.push({ header:header, value:value });
		return this;
	}

	/**
		Sets the parameter identified as `param` to value `value`.

		If `header` or `value` are null, the result is unspecified.

		This method provides a fluent interface.
	**/
	public function setParameter( param : String, value : String ):Http {
		params = Lambda.filter(params, function(p) return p.param != param);
		params.push({ param:param, value:value });
		return this;
	}

	public function addParameter( param : String, value : String ):Http {
		params.push({ param:param, value:value });
		return this;
	}

	/**
		Sets the post data of `this` Http request to `data`.

		There can only be one post data per request. Subsequent calls overwrite
		the previously set value.

		If `data` is null, the post data is considered to be absent.

		This method provides a fluent interface.
	**/
	public function setPostData( data : String ):Http {
		postData = data;
		return this;
	}

	#if (js || flash)

	#if nodejs
	var req:js.node.http.ClientRequest;
	#elseif js
	var req:js.html.XMLHttpRequest;
	#elseif flash
	var req:flash.net.URLLoader;
	#end

	/**
		Cancels `this` Http request if `request` has been called and a response
		has not yet been received.
	**/
	public function cancel()
	{
		if (req == null) return;
		#if js
		req.abort();
		#elseif flash
		req.close();
		#end
		req = null;
	}
	#end

	/**
		Sends `this` Http request to the Url specified by `this.url`.

		If `post` is true, the request is sent as POST request, otherwise it is
		sent as GET request.

		Depending on the outcome of the request, this method calls the
		`onStatus()`, `onError()` or `onData()` callback functions.

		If `this.url` is null, the result is unspecified.

		If `this.url` is an invalid or inaccessible Url, the `onError()` callback
		function is called.

		[js] If `this.async` is false, the callback functions are called before
		this method returns.
	**/
	public function request( ?post : Bool ) : Void {
		var me = this;
	#if nodejs
		me.responseData = null;
		var parsedUrl = js.node.Url.parse(url);
		var secure = (parsedUrl.protocol == "https:");
		var host = parsedUrl.hostname;
		var path = parsedUrl.path;
		var port = if (parsedUrl.port != null) Std.parseInt(parsedUrl.port) else (secure ? 443 : 80);
		var h:Dynamic = {};
		for (i in headers) {
			var arr = Reflect.field(h, i.header);
			if (arr == null) {
				arr = new Array<String>();
				Reflect.setField(h, i.header, arr);
			}
			
			arr.push(i.value);
		}
		var uri = postData;
		if( uri != null )
			post = true;
		else for( p in params ) {
			if( uri == null )
				uri = "";
			else
				uri += "&";
			uri += StringTools.urlEncode(p.param)+"="+StringTools.urlEncode(p.value);
		}
		var question = path.split("?").length <= 1;
		if (!post && uri != null) path += (if( question ) "?" else "&") + uri;
		
		var opts = {
			protocol: parsedUrl.protocol,
			hostname: host,
			port: port,
			method: post ? 'POST' : 'GET',
			path: path,
			headers: h
		};
		function httpResponse (res) {
			var s = res.statusCode;
			if (s != null)
				me.onStatus(s);
			var body = '';
			res.on('data', function (d) {
				body += d;
			});
			res.on('end', function (_) {
				me.responseData = body;
				me.req = null;
				if (s != null && s >= 200 && s < 400) {
					me.onData(body);
				} else {
					me.onError("Http Error #"+s);
				}
			});
		}
		req = secure ? js.node.Https.request(untyped opts, httpResponse) : js.node.Http.request(untyped opts, httpResponse);
		if (post) req.write(uri);
		req.end();
	#elseif js
		me.responseData = null;
		var r = req = js.Browser.createXMLHttpRequest();
		var onreadystatechange = function(_) {
			if( r.readyState != 4 )
				return;
			var s = try r.status catch( e : Dynamic ) null;
			if ( s != null && untyped __js__('"undefined" !== typeof window') ) {
				// If the request is local and we have data: assume a success (jQuery approach):
				var protocol = js.Browser.location.protocol.toLowerCase();
				var rlocalProtocol = ~/^(?:about|app|app-storage|.+-extension|file|res|widget):$/;
				var isLocal = rlocalProtocol.match( protocol );
				if ( isLocal ) {
					s = r.responseText != null ? 200 : 404;
				}
			}
			if( s == untyped __js__("undefined") )
				s = null;
			if( s != null )
				me.onStatus(s);
			if( s != null && s >= 200 && s < 400 ) {
				me.req = null;
				me.onData(me.responseData = r.responseText);
			}
			else if ( s == null ) {
				me.req = null;
				me.onError("Failed to connect or resolve host");
			}
			else switch( s ) {
			case 12029:
				me.req = null;
				me.onError("Failed to connect to host");
			case 12007:
				me.req = null;
				me.onError("Unknown host");
			default:
				me.req = null;
				me.responseData = r.responseText;
				me.onError("Http Error #"+r.status);
			}
		};
		if( async )
			r.onreadystatechange = onreadystatechange;
		var uri = postData;
		if( uri != null )
			post = true;
		else for( p in params ) {
			if( uri == null )
				uri = "";
			else
				uri += "&";
			uri += StringTools.urlEncode(p.param)+"="+StringTools.urlEncode(p.value);
		}
		try {
			if( post )
				r.open("POST",url,async);
			else if( uri != null ) {
				var question = url.split("?").length <= 1;
				r.open("GET",url+(if( question ) "?" else "&")+uri,async);
				uri = null;
			} else
				r.open("GET",url,async);
		} catch( e : Dynamic ) {
			me.req = null;
			onError(e.toString());
			return;
		}
		r.withCredentials = withCredentials;
		if( !Lambda.exists(headers, function(h) return h.header == "Content-Type") && post && postData == null )
			r.setRequestHeader("Content-Type","application/x-www-form-urlencoded");

		for( h in headers )
			r.setRequestHeader(h.header,h.value);
		r.send(uri);
		if( !async )
			onreadystatechange(null);
	#elseif flash
		me.responseData = null;
		var loader = req = new flash.net.URLLoader();
		loader.addEventListener( "complete", function(e) {
			me.req = null;
			me.responseData = loader.data;
			me.onData( loader.data );
		});
		loader.addEventListener( "httpStatus", function(e:flash.events.HTTPStatusEvent){
			// on Firefox 1.5, Flash calls onHTTPStatus with 0 (!??)
			if( e.status != 0 )
				me.onStatus( e.status );
		});
		loader.addEventListener( "ioError", function(e:flash.events.IOErrorEvent){
			me.req = null;
			me.responseData = loader.data;
			me.onError(e.text);
		});
		loader.addEventListener( "securityError", function(e:flash.events.SecurityErrorEvent){
			me.req = null;
			me.onError(e.text);
		});

		// headers
		var param = false;
		var vars = new flash.net.URLVariables();
		for( p in params ){
			param = true;
			Reflect.setField(vars,p.param,p.value);
		}
		var small_url = url;
		if( param && !post ){
			var k = url.split("?");
			if( k.length > 1 ) {
				small_url = k.shift();
				vars.decode(k.join("?"));
			}
		}
		// Bug in flash player 9 ???
		var bug = small_url.split("xxx");

		var request = new flash.net.URLRequest( small_url );
		for( h in headers )
			request.requestHeaders.push( new flash.net.URLRequestHeader(h.header,h.value) );

		if( postData != null ) {
			request.data = postData;
			request.method = "POST";
		} else {
			request.data = vars;
			request.method = if( post ) "POST" else "GET";
		}

		try {
			loader.load( request );
		}catch( e : Dynamic ){
			me.req = null;
			onError("Exception: "+Std.string(e));
		}
	#elseif sys
		var me = this;
		var output = new haxe.io.BytesOutput();
		var old = onError;
		var err = false;
		onError = function(e) {
			#if neko
			me.responseData = neko.Lib.stringReference(output.getBytes());
			#else
			me.responseData = output.getBytes().toString();
			#end
			err = true;
			// Resetting back onError before calling it allows for a second "retry" request to be sent without onError being wrapped twice
			onError = old;
			onError(e);
		}
		customRequest(post,output);
		if( !err )
		#if neko
			me.onData(me.responseData = neko.Lib.stringReference(output.getBytes()));
		#else
			me.onData(me.responseData = output.getBytes().toString());
		#end
	#end
	}

#if sys

	/**
      Note: Deprecated in 4.0
	 **/
	@:noCompletion
	inline public function fileTransfert( argname : String, filename : String, file : haxe.io.Input, size : Int, mimeType = "application/octet-stream" ) {
	    fileTransfer(argname, filename, file, size, mimeType);
    }

	public function fileTransfer( argname : String, filename : String, file : haxe.io.Input, size : Int, mimeType = "application/octet-stream" ) {
		this.file = { param : argname, filename : filename, io : file, size : size, mimeType : mimeType };
	}

	public function customRequest( post : Bool, api : haxe.io.Output, ?sock : sys.net.Socket, ?method : String  ) {
		this.responseData = null;
		var url_regexp = ~/^(https?:\/\/)?([a-zA-Z\.0-9_-]+)(:[0-9]+)?(.*)$/;
		if( !url_regexp.match(url) ) {
			onError("Invalid URL");
			return;
		}
		var secure = (url_regexp.matched(1) == "https://");
		if( sock == null ) {
			if( secure ) {
				#if php
				sock = new php.net.SslSocket();
				#elseif java
				sock = new java.net.SslSocket();
				#elseif (!no_ssl && (hxssl || hl || cpp || (neko && !(macro || interp))))
				sock = new sys.ssl.Socket();
				#else
				throw "Https is only supported with -lib hxssl";
				#end
			} else
				sock = new Socket();
		}
		var host = url_regexp.matched(2);
		var portString = url_regexp.matched(3);
		var request = url_regexp.matched(4);
		if( request == "" )
			request = "/";
		var port = if ( portString == null || portString == "" ) secure ? 443 : 80 else Std.parseInt(portString.substr(1, portString.length - 1));
		var data;

		var multipart = (file != null);
		var boundary = null;
		var uri = null;
		if( multipart ) {
			post = true;
			boundary = Std.string(Std.random(1000))+Std.string(Std.random(1000))+Std.string(Std.random(1000))+Std.string(Std.random(1000));
			while( boundary.length < 38 )
				boundary = "-" + boundary;
			var b = new StringBuf();
			for( p in params ) {
				b.add("--");
				b.add(boundary);
				b.add("\r\n");
				b.add('Content-Disposition: form-data; name="');
				b.add(p.param);
				b.add('"');
				b.add("\r\n");
				b.add("\r\n");
				b.add(p.value);
				b.add("\r\n");
			}
			b.add("--");
			b.add(boundary);
			b.add("\r\n");
			b.add('Content-Disposition: form-data; name="');
			b.add(file.param);
			b.add('"; filename="');
			b.add(file.filename);
			b.add('"');
			b.add("\r\n");
			b.add("Content-Type: "+file.mimeType+"\r\n"+"\r\n");
			uri = b.toString();
		} else {
			for( p in params ) {
				if( uri == null )
					uri = "";
				else
					uri += "&";
				uri += StringTools.urlEncode(p.param)+"="+StringTools.urlEncode(p.value);
			}
		}

		var b = new StringBuf();
		if( method != null ) {
			b.add(method);
			b.add(" ");
		} else if( post )
			b.add("POST ");
		else
			b.add("GET ");

		if( Http.PROXY != null ) {
			b.add("http://");
			b.add(host);
			if( port != 80 ) {
				b.add(":");
				b.add(port);
			}
		}
		b.add(request);

		if( !post && uri != null ) {
			if( request.indexOf("?",0) >= 0 )
				b.add("&");
			else
				b.add("?");
			b.add(uri);
		}
		b.add(" HTTP/1.1\r\nHost: "+host+"\r\n");
		if( postData != null )
			b.add("Content-Length: "+postData.length+"\r\n");
		else if( post && uri != null ) {
			if( multipart || !Lambda.exists(headers, function(h) return h.header == "Content-Type") ) {
				b.add("Content-Type: ");
				if( multipart ) {
					b.add("multipart/form-data");
					b.add("; boundary=");
					b.add(boundary);
				} else
					b.add("application/x-www-form-urlencoded");
				b.add("\r\n");
			}
			if( multipart )
				b.add("Content-Length: "+(uri.length+file.size+boundary.length+6)+"\r\n");
			else
				b.add("Content-Length: "+uri.length+"\r\n");
		}
		b.add("Connection: close\r\n");
		for( h in headers ) {
			b.add(h.header);
			b.add(": ");
			b.add(h.value);
			b.add("\r\n");
		}
		b.add("\r\n");
		if( postData != null)
			b.add(postData);
		else if( post && uri != null )
			b.add(uri);
		try {
			if( Http.PROXY != null )
				sock.connect(new Host(Http.PROXY.host),Http.PROXY.port);
			else
				sock.connect(new Host(host),port);
			sock.write(b.toString());
			if( multipart ) {
				var bufsize = 4096;
				var buf = haxe.io.Bytes.alloc(bufsize);
				while( file.size > 0 ) {
					var size = if( file.size > bufsize ) bufsize else file.size;
					var len = 0;
					try {
						len = file.io.readBytes(buf,0,size);
					} catch( e : haxe.io.Eof ) break;
					sock.output.writeFullBytes(buf,0,len);
					file.size -= len;
				}
				sock.write("\r\n");
				sock.write("--");
				sock.write(boundary);
				sock.write("--");
			}
			readHttpResponse(api,sock);
			sock.close();
		} catch( e : Dynamic ) {
			try sock.close() catch( e : Dynamic ) { };
			onError(Std.string(e));
		}
	}

	function readHttpResponse( api : haxe.io.Output, sock : sys.net.Socket ) {
		// READ the HTTP header (until \r\n\r\n)
		var b = new haxe.io.BytesBuffer();
		var k = 4;
		var s = haxe.io.Bytes.alloc(4);
		sock.setTimeout(cnxTimeout);
		while( true ) {
			var p = sock.input.readBytes(s,0,k);
			while( p != k )
				p += sock.input.readBytes(s,p,k - p);
			b.addBytes(s,0,k);
			switch( k ) {
			case 1:
				var c = s.get(0);
				if( c == 10 )
					break;
				if( c == 13 )
					k = 3;
				else
					k = 4;
			case 2:
				var c = s.get(1);
				if( c == 10 ) {
					if( s.get(0) == 13 )
						break;
					k = 4;
				} else if( c == 13 )
					k = 3;
				else
					k = 4;
			case 3:
				var c = s.get(2);
				if( c == 10 ) {
					if( s.get(1) != 13 )
						k = 4;
					else if( s.get(0) != 10 )
						k = 2;
					else
						break;
				} else if( c == 13 ) {
					if( s.get(1) != 10 || s.get(0) != 13 )
						k = 1;
					else
						k = 3;
				} else
					k = 4;
			case 4:
				var c = s.get(3);
				if( c == 10 ) {
					if( s.get(2) != 13 )
						continue;
					else if( s.get(1) != 10 || s.get(0) != 13 )
						k = 2;
					else
						break;
				} else if( c == 13 ) {
					if( s.get(2) != 10 || s.get(1) != 13 )
						k = 3;
					else
						k = 1;
				}
			}
		}
		#if neko
		var headers = neko.Lib.stringReference(b.getBytes()).split("\r\n");
		#else
		var headers = b.getBytes().toString().split("\r\n");
		#end
		var response = headers.shift();
		var rp = response.split(" ");
		var status = Std.parseInt(rp[1]);
		if( status == 0 || status == null )
			throw "Response status error";

		// remove the two lasts \r\n\r\n
		headers.pop();
		headers.pop();
		responseHeaders = new haxe.ds.StringMap();
		var size = null;
		var chunked = false;
		for( hline in headers ) {
			var a = hline.split(": ");
			var hname = a.shift();
			var hval = if( a.length == 1 ) a[0] else a.join(": ");
			hval = StringTools.ltrim( StringTools.rtrim( hval ) );
			responseHeaders.set(hname, hval);
			switch(hname.toLowerCase())
			{
				case "content-length":
					size = Std.parseInt(hval);
				case "transfer-encoding":
					chunked = (hval.toLowerCase() == "chunked");
			}
		}

		onStatus(status);

		var chunk_re = ~/^([0-9A-Fa-f]+)[ ]*\r\n/m;
		chunk_size = null;
		chunk_buf = null;

		var bufsize = 1024;
		var buf = haxe.io.Bytes.alloc(bufsize);
		if( chunked ) {
			try {
				while( true ) {
					var len = sock.input.readBytes(buf,0,bufsize);
					if( !readChunk(chunk_re,api,buf,len) )
						break;
				}
			} catch ( e : haxe.io.Eof ) {
				throw "Transfer aborted";
			}
		} else if( size == null ) {
			if( !noShutdown )
				sock.shutdown(false,true);
			try {
				while( true ) {
					var len = sock.input.readBytes(buf,0,bufsize);
					api.writeBytes(buf,0,len);
				}
			} catch( e : haxe.io.Eof ) {
			}
		} else {
			api.prepare(size);
			try {
				while( size > 0 ) {
					var len = sock.input.readBytes(buf,0,if( size > bufsize ) bufsize else size);
					api.writeBytes(buf,0,len);
					size -= len;
				}
			} catch( e : haxe.io.Eof ) {
				throw "Transfer aborted";
			}
		}
		if( chunked && (chunk_size != null || chunk_buf != null) )
			throw "Invalid chunk";
		if( status < 200 || status >= 400 )
			throw "Http Error #"+status;
		api.close();
	}

	function readChunk(chunk_re : EReg, api : haxe.io.Output, buf : haxe.io.Bytes, len ) {
		if( chunk_size == null ) {
			if( chunk_buf != null ) {
				var b = new haxe.io.BytesBuffer();
				b.add(chunk_buf);
				b.addBytes(buf,0,len);
				buf = b.getBytes();
				len += chunk_buf.length;
				chunk_buf = null;
			}
			#if neko
			if( chunk_re.match(neko.Lib.stringReference(buf)) ) {
			#else
			if( chunk_re.match(buf.toString()) ) {
			#end
				var p = chunk_re.matchedPos();
				if( p.len <= len ) {
					var cstr = chunk_re.matched(1);
					chunk_size = Std.parseInt("0x"+cstr);
					if( cstr == "0" ) {
						chunk_size = null;
						chunk_buf = null;
						return false;
					}
					len -= p.len;
					return readChunk(chunk_re,api,buf.sub(p.len,len),len);
				}
			}
			// prevent buffer accumulation
			if( len > 10 ) {
				onError("Invalid chunk");
				return false;
			}
			chunk_buf = buf.sub(0,len);
			return true;
		}
		if( chunk_size > len ) {
			chunk_size -= len;
			api.writeBytes(buf,0,len);
			return true;
		}
		var end = chunk_size + 2;
		if( len >= end ) {
			if( chunk_size > 0 )
				api.writeBytes(buf,0,chunk_size);
			len -= end;
			chunk_size = null;
			if( len == 0 )
				return true;
			return readChunk(chunk_re,api,buf.sub(end,len),len);
		}
		if( chunk_size > 0 )
			api.writeBytes(buf,0,chunk_size);
		chunk_size -= len;
		return true;
	}

#end

	/**
		This method is called upon a successful request, with `data` containing
		the result String.

		The intended usage is to bind it to a custom function:  
		`httpInstance.onData = function(data) { // handle result }`
	**/
	public dynamic function onData( data : String ) {
	}

	/**
		This method is called upon a request error, with `msg` containing the
		error description.

		The intended usage is to bind it to a custom function:  
		`httpInstance.onError = function(msg) { // handle error }`
	**/
	public dynamic function onError( msg : String ) {
	}

	/**
		This method is called upon a Http status change, with `status` being the
		new status.

		The intended usage is to bind it to a custom function:  
		`httpInstance.onStatus = function(status) { // handle status }`
	**/
	public dynamic function onStatus( status : Int ) {
	}

#if (!flash && !nodejs)
	/**
		Makes a synchronous request to `url`.

		This creates a new Http instance and makes a GET request by calling its
		`request(false)` method.

		If `url` is null, the result is unspecified.
	**/
	public static function requestUrl( url : String ) : String {
		var h = new Http(url);
	#if js
		h.async = false;
	#end
		var r = null;
		h.onData = function(d){
			r = d;
		}
		h.onError = function(e){
			throw e;
		}
		h.request(false);
		return r;
	}
#end

}
