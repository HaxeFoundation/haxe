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
		var port = if( portString == "" ) 80 else Std.parseInt(portString.substr(1,-1));
		var s = new neko.Socket();
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
		if( post && uri != null ) {
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
		b.add("\r\n");
		if( post && uri != null )
			b.add(uri);
		try {
			s.connect(neko.Socket.resolve(host),port);
			s.write(b.toString());
			s.waitForRead();
			s.shutdown(false,true);
			data = s.read();
			s.close();
		} catch( e : Dynamic ) {
			onError(Std.string(e));
			return;
		}
		responseHeaders = new Hash();
		var crlf = ~/\r\n\r\n/;
		if( !crlf.match(data) ) {
			onData(data);
			return;
		}
		var header_end = crlf.matchedPos();
		var headers = data.substr(0,header_end.pos).split("\r\n");
		var response = headers.shift();
		for( hline in headers ) {
			var a = hline.split(": ");
			var hname = a.shift();
			if( a.length == 1 )
				responseHeaders.set(hname,a[0]);
			else
				responseHeaders.set(hname,a.join(": "));
		}
		var body = data.substr(header_end.pos+4,data.length-(header_end.pos+4));
		if( responseHeaders.get("Transfer-Encoding") == "chunked" )
			body = unchunk(body);
		onData(body);
	#end
	}

#if neko
	static function unchunk( s : String ) : String {
		var chunk = ~/^([0-9A-Fa-f]+)[ ]*\r\n/;
		if( !chunk.match(s) )
			return s;
		var m = chunk.matched(1);
		var l = chunk.matched(0).length;
		var n = Std.parseInt("0x"+m);
		var sz = l+n+2;
		return s.substr(l,n) + unchunk(s.substr(sz,s.length-sz));
	}
#end

	public function onData( data : String ) {
	}

	public function onError( msg : String ) {
	}

	public function onStatus( status : Int ) {
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
