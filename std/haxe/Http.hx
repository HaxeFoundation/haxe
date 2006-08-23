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
package haxe;

class Http {

	public var url : String;
#if neko
	var responseHeaders : Hash<String>;
	var postData : String;
	var chunk_size : Int;
	var chunk_buf : String;
#else js
	private var async : Bool;
#end
	var headers : Hash<String>;
	var params : Hash<String>;

	public function new( url : String ) {
		this.url = url;
		headers = new Hash();
		params = new Hash();
		#if js
		async = true;
		#end
	}

	public function setHeader( header : String, value : String ) {
		headers.set(header,value);
	}

	public function setParameter( param : String, value : String ) {
		params.set(param,value);
	}

	public function request( post : Bool ) : Void {
		var me = this;
	#if js
		var r = new js.XMLHttpRequest();
		var onreadystatechange = function() {
			if( r.readyState != 4 )
				return;
			var s = try r.status catch( e : Dynamic ) null;
			if( s == untyped __js__("undefined") )
				s = null;
			if( s != null )
				me.onStatus(s);
			if( s != null && s >= 200 && s < 400 )
				me.onData(r.responseText);
			else switch( s ) {
			case null:
				me.onError("Failed to connect or resolve host");
			case 12029:
				me.onError("Failed to connect to host");
			case 12007:
				me.onError("Unknown host");
			default:
				me.onError("Http Error #"+r.status);
			}
		};
		r.onreadystatechange = onreadystatechange;
		var uri = null;
		for( p in params.keys() ) {
			if( uri == null )
				uri = "";
			else
				uri += "&";
			uri += StringTools.urlDecode(p)+"="+StringTools.urlEncode(params.get(p));
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
			onError(e.toString());
			return;
		}
		if( headers.get("Content-Type") == null && post )
			r.setRequestHeader("Content-Type","application/x-www-form-urlencoded");

		for( h in headers.keys() )
			r.setRequestHeader(h,headers.get(h));
		r.send(uri);
		if( !async )
			onreadystatechange();
	#else flash9
		var loader = new flash.net.URLLoader();
		loader.addEventListener( "complete", function(e){
			me.onData( loader.data );
		});
		loader.addEventListener( "httpStatus", function(e){
			// on Firefox 1.5, Flash calls onHTTPStatus with 0 (!??)
			if( e.status != 0 )
				me.onStatus( e.status );
		});
		loader.addEventListener( "ioError", function(e){
			me.onError(e.text);
		});
		loader.addEventListener( "securityError", function(e){
			me.onError(e.text);
		});

		// headers
		var param = false;
		var vars = new flash.net.URLVariables();
		for( k in params.keys() ){
			param = true;
			Reflect.setField(vars,k,params.get(k));
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
		for( k in headers.keys() ){
			request.requestHeaders.push( new flash.net.URLRequestHeader(k,headers.get(k)) );
		}

		request.data = vars;
		request.method = if( post ) "POST" else "GET";

		try {
			loader.load( request );
		}catch( e : Dynamic ){
			onError("Exception: "+Std.string(e));
		}
	#else flash
		var r = new flash.LoadVars();
		// on Firefox 1.5, onData is not called if host/port invalid (!)
		r.onData = function(data) {
			if( data == null ) {
				me.onError("Failed to retrieve url");
				return;
			}
			me.onData(data);
		};
		#if flash8
		r.onHTTPStatus = function(status) {
			// on Firefox 1.5, Flash calls onHTTPStatus with 0 (!??)
			if( status != 0 )
				me.onStatus(status);
		};
		untyped ASSetPropFlags(r,"onHTTPStatus",7);
		#end
		untyped ASSetPropFlags(r,"onData",7);
		for( h in headers.keys() )
			r.addRequestHeader(h,headers.get(h));
		var param = false;
		for( p in params.keys() ) {
			param = true;
			Reflect.setField(r,p,params.get(p));
		}
		var small_url = url;
		if( param && !post ) {
			var k = url.split("?");
			if( k.length > 1 ) {
				small_url = k.shift();
				r.decode(k.join("?"));
			}
		}
		if( !r.sendAndLoad(small_url,r,if( param ) { if( post ) "POST" else "GET"; } else null) )
			onError("Failed to initialize Connection");
	#else neko
		var me = this;
		var output = new neko.io.StringOutput();
		output.close = function() {
			me.onData(output.toString());
		};
		asyncRequest(post,output);
	#end
	}

#if neko

	public function asyncRequest( post : Bool, api : neko.io.Output ) {
		var url_regexp = ~/^(http:\/\/)?([a-zA-Z\.0-9-]+)(:[0-9]+)?(.*)$/;
		if( !url_regexp.match(url) ) {
			onError("Invalid URL");
			return;
		}
		var host = url_regexp.matched(2);
		var portString = url_regexp.matched(3);
		var request = url_regexp.matched(4);
		if( request == "" )
			request = "/";
		var port = if( portString == "" ) 80 else Std.parseInt(portString.substr(1,portString.length-1));
		var s = new neko.io.Socket();
		var data;

		var uri = null;
		for( p in params.keys() ) {
			if( uri == null )
				uri = "";
			else
				uri += "&";
			uri += StringTools.urlDecode(p)+"="+StringTools.urlEncode(params.get(p));
		}

		var b = new StringBuf();
		if( post )
			b.add("POST ");
		else
			b.add("GET ");
		b.add(request);
		if( !post && uri != null ) {
			if( request.indexOf("?",0) >= 0 )
				b.add("&");
			else
				b.add("?");
			b.add(uri);
		}
		b.add(" HTTP/1.1\r\nHost: "+host+"\r\n");
		if( postData == null && post && uri != null ) {
			if( headers.get("Content-Type") == null )
				b.add("Content-Type: "+"application/x-www-form-urlencoded"+"\r\n");
			b.add("Content-Length: "+uri.length+"\r\n");
		}
		for( h in headers.keys() ) {
			b.add(h);
			b.add(": ");
			b.add(headers.get(h));
			b.add("\r\n");
		}
		if( postData != null)
			b.add(postData);
		else {
			b.add("\r\n");
			if( post && uri != null )
				b.add(uri);
		}
		try {
			s.connect(neko.io.Socket.resolve(host),port);
			s.write(b.toString());
			readHttpResponse(api,s);
			s.close();
		} catch( e : Dynamic ) {
			onError(Std.string(e));
		}
	}

	function readHttpResponse( api : neko.io.Output, sock : neko.io.Socket ) {
		// READ the HTTP header (until \r\n\r\n)
		var b = new StringBuf();
		var k = 4;
		var s = neko.Lib.makeString(4);
		sock.setTimeout(10); // 10 seconds
		while( true ) {
			var p = sock.input.readBytes(s,0,k);
			while( p != k )
				p += sock.input.readBytes(s,p,k - p);
			b.addSub(s,0,k);
			switch( k ) {
			case 1:
				var c = s.charCodeAt(0);
				if( c == 10 )
					break;
				if( c == 13 )
					k = 3;
				else
					k = 4;
			case 2:
				var c = s.charCodeAt(1);
				if( c == 10 ) {
					if( s.charCodeAt(0) == 13 )
						break;
					k = 4;
				} else if( c == 13 )
					k = 3;
				else
					k = 4;
			case 3:
				var c = s.charCodeAt(2);
				if( c == 10 ) {
					if( s.charCodeAt(1) != 13 )
						k = 4;
					else if( s.charCodeAt(0) != 10 )
						k = 2;
					else
						break;
				} else if( c == 13 ) {
					if( s.charCodeAt(1) != 10 || s.charCodeAt(0) != 13 )
						k = 1;
					else
						k = 3;
				} else
					k = 4;
			case 4:
				var c = s.charCodeAt(3);
				if( c == 10 ) {
					if( s.charCodeAt(2) != 13 )
						continue;
					else if( s.charCodeAt(1) != 10 || s.charCodeAt(0) != 13 )
						k = 2;
					else
						break;
				} else if( c == 13 ) {
					if( s.charCodeAt(2) != 10 || s.charCodeAt(1) != 13 )
						k = 3;
					else
						k = 1;
				}
			}
		}
		var headers = b.toString().split("\r\n");
		var response = headers.shift();
		var rp = response.split(" ");
		var status = Std.parseInt(rp[1]);

		if( status == 0 || status == null ){
			onError("Response status error");
			return;
		}
		onStatus(status);
		if( status < 200 || status >= 400 ){
			onError("Http Error #"+status);
			return;
		}

		// remove the two lasts \r\n\r\n
		headers.pop();
		headers.pop();
		responseHeaders = new Hash();
		for( hline in headers ) {
			var a = hline.split(": ");
			var hname = a.shift();
			if( a.length == 1 )
				responseHeaders.set(hname,a[0]);
			else
				responseHeaders.set(hname,a.join(": "));
		}
		var size = Std.parseInt(responseHeaders.get("Content-Length"));
		var chunked = responseHeaders.get("Transfer-Encoding") == "chunked";
		var chunk_re = ~/^([0-9A-Fa-f]+)[ ]*\r\n/m;
		chunk_size = null;
		chunk_buf = null;

		var bufsize = 1024;
		var buf = neko.Lib.makeString(bufsize);
		api.prepare(size);
		if( size == null ) {
			sock.shutdown(false,true);
			try {
				while( true ) {
					var len = sock.input.readBytes(buf,0,bufsize);
					if( chunked ) {
						if( !readChunk(chunk_re,api,buf,len) )
							break;
					} else
						api.writeBytes(buf,0,len);
				}
			} catch( e : neko.io.Eof ) {
			} catch( e : Dynamic ) {
				onError(e);
				return;
			}
		} else {
			try {
				while( size > 0 ) {
					var len = sock.input.readBytes(buf,0,if( size > bufsize ) bufsize else size);
					if( chunked ) {
						if( !readChunk(chunk_re,api,buf,len) )
							break;
					} else
						api.writeBytes(buf,0,len);
					size -= len;
				}
			} catch( e : neko.io.Eof ) {
				onError("Transfert aborted");
				return;
			} catch( e : Dynamic ) {
				onError(e);
				return;
			}
		}
		if( chunked && (chunk_size != null || chunk_buf != null) ) {
			onError("Invalid chunk");
			return;
		}
		api.close();
	}

	function readChunk(chunk_re : EReg, api : neko.io.Output, buf : String, len ) {
		if( chunk_size == null ) {
			if( chunk_buf != null ) {
				buf = chunk_buf + buf.substr(0,len);
				len += chunk_buf.length;
				chunk_buf = null;
			}
			if( chunk_re.match(buf) ) {
				var p = chunk_re.matchedPos();
				if( p.len <= len ) {
					chunk_size = Std.parseInt("0x"+chunk_re.matched(1));
					len -= p.len;
					readChunk(chunk_re,api,buf.substr(p.len,len),len);
					return true;
				}
			}
			// prevent buffer accumulation
			if( len > 10 ) {
				onError("Invalid chunk");
				return false;
			}
			chunk_buf = buf.substr(0,len);
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
			return readChunk(chunk_re,api,buf.substr(end,len),len);
		}
		if( chunk_size > 0 )
			api.writeBytes(buf,0,chunk_size);
		chunk_size -= len;
		return true;
	}

#end

	public f9dynamic function onData( data : String ) {
	}

	public f9dynamic function onError( msg : String ) {
	}

	public f9dynamic function onStatus( status : Int ) {
	}

#if flash
#else true
	public static function request( url : String ) : String {
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
