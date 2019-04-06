/*
 * Copyright (C)2005-2019 Haxe Foundation
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
package sys;

import sys.net.Host;
import sys.net.Socket;

class Http extends haxe.http.HttpBase {
	public var noShutdown:Bool;
	public var cnxTimeout:Float;
	public var responseHeaders:Map<String,String>;
	var chunk_size:Null<Int>;
	var chunk_buf:haxe.io.Bytes;
	var file:{ param:String, filename:String, io:haxe.io.Input, size:Int, mimeType:String };

	public static var PROXY:{ host:String, port:Int, auth:{ user:String, pass:String } } = null;

	public function new(url:String) {
		cnxTimeout = 10;
		#if php
		noShutdown = !php.Global.function_exists('stream_socket_shutdown');
		#end
		super(url);
	}

	public override function request(?post:Bool) {
		var output = new haxe.io.BytesOutput();
		var old = onError;
		var err = false;
		onError = function(e) {
			#if neko
			responseData = neko.Lib.stringReference(output.getBytes());
			#else
			responseData = output.getBytes().toString();
			#end
			err = true;
			// Resetting back onError before calling it allows for a second "retry" request to be sent without onError being wrapped twice
			onError = old;
			onError(e);
		}
		customRequest(post,output);
		if(!err)
		#if neko
			onData(responseData = neko.Lib.stringReference(output.getBytes()));
		#else
			onData(responseData = output.getBytes().toString());
		#end
	}

	@:noCompletion
	@:deprecated("Use fileTransfer instead")
	inline public function fileTransfert(argname:String, filename:String, file:haxe.io.Input, size:Int, mimeType = "application/octet-stream") {
	    fileTransfer(argname, filename, file, size, mimeType);
    }

	public function fileTransfer(argname:String, filename:String, file:haxe.io.Input, size:Int, mimeType = "application/octet-stream") {
		this.file = { param:argname, filename:filename, io:file, size:size, mimeType:mimeType };
	}

	public function customRequest(post:Bool, api:haxe.io.Output, ?sock:sys.net.Socket, ?method:String) {
		this.responseData = null;
		var url_regexp = ~/^(https?:\/\/)?([a-zA-Z\.0-9_-]+)(:[0-9]+)?(.*)$/;
		if(!url_regexp.match(url)) {
			onError("Invalid URL");
			return;
		}
		var secure = (url_regexp.matched(1) == "https://");
		if(sock == null) {
			if(secure) {
				#if php
				sock = new php.net.SslSocket();
				#elseif java
				sock = new java.net.SslSocket();
				#elseif python
				sock = new python.net.SslSocket();
				#elseif (!no_ssl && (hxssl || hl || cpp || (neko && !(macro || interp))))
				sock = new sys.ssl.Socket();
				#else
				throw "Https is only supported with -lib hxssl";
				#end
			} else {
				#if php
				sock = new php.net.Socket();
				#else
				sock = new Socket();
				#end
			}
		}
		var host = url_regexp.matched(2);
		var portString = url_regexp.matched(3);
		var request = url_regexp.matched(4);
		// ensure path begins with a forward slash
		// this is required by original URL specifications and many servers have issues if it's not supplied
		// see https://stackoverflow.com/questions/1617058/ok-to-skip-slash-before-query-string
		if (request.charAt(0) != "/") {
			request = "/" + request;
		}
		var port = if (portString == null || portString == "") secure ? 443:80 else Std.parseInt(portString.substr(1, portString.length - 1));

		var multipart = (file != null);
		var boundary = null;
		var uri = null;
		if(multipart) {
			post = true;
			boundary = Std.string(Std.random(1000))+Std.string(Std.random(1000))+Std.string(Std.random(1000))+Std.string(Std.random(1000));
			while(boundary.length < 38)
				boundary = "-" + boundary;
			var b = new StringBuf();
			for(p in params) {
				b.add("--");
				b.add(boundary);
				b.add("\r\n");
				b.add('Content-Disposition: form-data; name="');
				b.add(p.name);
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
			for(p in params) {
				if(uri == null)
					uri = "";
				else
					uri += "&";
				uri += StringTools.urlEncode(p.name)+"="+StringTools.urlEncode(p.value);
			}
		}

		var b = new StringBuf();
		if(method != null) {
			b.add(method);
			b.add(" ");
		} else if(post)
			b.add("POST ");
		else
			b.add("GET ");

		if(Http.PROXY != null) {
			b.add("http://");
			b.add(host);
			if(port != 80) {
				b.add(":");
				b.add(port);
			}
		}
		b.add(request);

		if(!post && uri != null) {
			if(request.indexOf("?",0) >= 0)
				b.add("&");
			else
				b.add("?");
			b.add(uri);
		}
		b.add(" HTTP/1.1\r\nHost: "+host+"\r\n");
		if(postData != null)
			b.add("Content-Length: "+postData.length+"\r\n");
		else if(post && uri != null) {
			if(multipart || !Lambda.exists(headers, function(h) return h.name == "Content-Type")) {
				b.add("Content-Type: ");
				if(multipart) {
					b.add("multipart/form-data");
					b.add("; boundary=");
					b.add(boundary);
				} else
					b.add("application/x-www-form-urlencoded");
				b.add("\r\n");
			}
			if(multipart)
				b.add("Content-Length: "+(uri.length+file.size+boundary.length+6)+"\r\n");
			else
				b.add("Content-Length: "+uri.length+"\r\n");
		}
		b.add("Connection: close\r\n");
		for(h in headers) {
			b.add(h.name);
			b.add(": ");
			b.add(h.value);
			b.add("\r\n");
		}
		b.add("\r\n");
		if(postData != null)
			b.add(postData);
		else if(post && uri != null)
			b.add(uri);
		try {
			if(Http.PROXY != null)
				sock.connect(new Host(Http.PROXY.host),Http.PROXY.port);
			else
				sock.connect(new Host(host),port);
			sock.write(b.toString());
			if(multipart) {
				var bufsize = 4096;
				var buf = haxe.io.Bytes.alloc(bufsize);
				while(file.size > 0) {
					var size = if(file.size > bufsize) bufsize else file.size;
					var len = 0;
					try {
						len = file.io.readBytes(buf,0,size);
					} catch(e:haxe.io.Eof) break;
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
		} catch(e:Dynamic) {
			try sock.close() catch(e:Dynamic) { };
			onError(Std.string(e));
		}
	}

	function readHttpResponse(api:haxe.io.Output, sock:sys.net.Socket) {
		// READ the HTTP header (until \r\n\r\n)
		var b = new haxe.io.BytesBuffer();
		var k = 4;
		var s = haxe.io.Bytes.alloc(4);
		sock.setTimeout(cnxTimeout);
		while(true) {
			var p = sock.input.readBytes(s,0,k);
			while(p != k)
				p += sock.input.readBytes(s,p,k - p);
			b.addBytes(s,0,k);
			switch(k) {
			case 1:
				var c = s.get(0);
				if(c == 10)
					break;
				if(c == 13)
					k = 3;
				else
					k = 4;
			case 2:
				var c = s.get(1);
				if(c == 10) {
					if(s.get(0) == 13)
						break;
					k = 4;
				} else if(c == 13)
					k = 3;
				else
					k = 4;
			case 3:
				var c = s.get(2);
				if(c == 10) {
					if(s.get(1) != 13)
						k = 4;
					else if(s.get(0) != 10)
						k = 2;
					else
						break;
				} else if(c == 13) {
					if(s.get(1) != 10 || s.get(0) != 13)
						k = 1;
					else
						k = 3;
				} else
					k = 4;
			case 4:
				var c = s.get(3);
				if(c == 10) {
					if(s.get(2) != 13)
						continue;
					else if(s.get(1) != 10 || s.get(0) != 13)
						k = 2;
					else
						break;
				} else if(c == 13) {
					if(s.get(2) != 10 || s.get(1) != 13)
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
		if(status == 0 || status == null)
			throw "Response status error";

		// remove the two lasts \r\n\r\n
		headers.pop();
		headers.pop();
		responseHeaders = new haxe.ds.StringMap();
		var size = null;
		var chunked = false;
		for(hline in headers) {
			var a = hline.split(": ");
			var hname = a.shift();
			var hval = if(a.length == 1) a[0] else a.join(": ");
			hval = StringTools.ltrim(StringTools.rtrim(hval));
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
		if(chunked) {
			try {
				while(true) {
					var len = sock.input.readBytes(buf,0,bufsize);
					if(!readChunk(chunk_re,api,buf,len))
						break;
				}
			} catch (e:haxe.io.Eof) {
				throw "Transfer aborted";
			}
		} else if(size == null) {
			if(!noShutdown)
				sock.shutdown(false,true);
			try {
				while(true) {
					var len = sock.input.readBytes(buf,0,bufsize);
					if (len == 0) break;
					api.writeBytes(buf,0,len);
				}
			} catch(e:haxe.io.Eof) {
			}
		} else {
			api.prepare(size);
			try {
				while(size > 0) {
					var len = sock.input.readBytes(buf,0,if(size > bufsize) bufsize else size);
					api.writeBytes(buf,0,len);
					size -= len;
				}
			} catch(e:haxe.io.Eof) {
				throw "Transfer aborted";
			}
		}
		if(chunked && (chunk_size != null || chunk_buf != null))
			throw "Invalid chunk";
		if(status < 200 || status >= 400)
			throw "Http Error #"+status;
		api.close();
	}

	function readChunk(chunk_re:EReg, api:haxe.io.Output, buf:haxe.io.Bytes, len) {
		if(chunk_size == null) {
			if(chunk_buf != null) {
				var b = new haxe.io.BytesBuffer();
				b.add(chunk_buf);
				b.addBytes(buf,0,len);
				buf = b.getBytes();
				len += chunk_buf.length;
				chunk_buf = null;
			}
			#if neko
			if(chunk_re.match(neko.Lib.stringReference(buf))) {
			#else
			if(chunk_re.match(buf.toString())) {
			#end
				var p = chunk_re.matchedPos();
				if(p.len <= len) {
					var cstr = chunk_re.matched(1);
					chunk_size = Std.parseInt("0x"+cstr);
					if(chunk_size == 0) {
						chunk_size = null;
						chunk_buf = null;
						return false;
					}
					len -= p.len;
					return readChunk(chunk_re,api,buf.sub(p.len,len),len);
				}
			}
			// prevent buffer accumulation
			if(len > 10) {
				onError("Invalid chunk");
				return false;
			}
			chunk_buf = buf.sub(0,len);
			return true;
		}
		if(chunk_size > len) {
			chunk_size -= len;
			api.writeBytes(buf,0,len);
			return true;
		}
		var end = chunk_size + 2;
		if(len >= end) {
			if(chunk_size > 0)
				api.writeBytes(buf,0,chunk_size);
			len -= end;
			chunk_size = null;
			if(len == 0)
				return true;
			return readChunk(chunk_re,api,buf.sub(end,len),len);
		}
		if(chunk_size > 0)
			api.writeBytes(buf,0,chunk_size);
		chunk_size -= len;
		return true;
	}

	/**
		Makes a synchronous request to `url`.

		This creates a new Http instance and makes a GET request by calling its
		`request(false)` method.

		If `url` is null, the result is unspecified.
	**/
	public static function requestUrl(url:String):String {
		var h = new Http(url);
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
}
