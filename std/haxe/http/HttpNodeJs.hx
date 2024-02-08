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

package haxe.http;

#if nodejs
import js.node.Buffer;
import haxe.io.Bytes;

class HttpNodeJs extends haxe.http.HttpBase {
	public var responseHeaders:Map<String, String>;

	var req:js.node.http.ClientRequest;

	public function new(url:String) {
		super(url);
	}

	/**
		Cancels `this` Http request if `request` has been called and a response
		has not yet been received.
	**/
	public function cancel() {
		if (req == null)
			return;
		req.abort();
		req = null;
	}

	public override function request(?post:Bool) {
		responseAsString = null;
		responseBytes = null;
		responseHeaders = null;
		var parsedUrl = new js.node.url.URL(url);
		var secure = (parsedUrl.protocol == "https:");
		var host = parsedUrl.hostname;
		var path = parsedUrl.pathname;
		var port = if (parsedUrl.port != null) Std.parseInt(parsedUrl.port) else (secure ? 443 : 80);
		var h:Dynamic = {};
		for (i in headers) {
			var arr = Reflect.field(h, i.name);
			if (arr == null) {
				arr = new Array<String>();
				Reflect.setField(h, i.name, arr);
			}

			arr.push(i.value);
		}
		if (postData != null || postBytes != null)
			post = true;
		var uri = null;
		for (p in params) {
			if (uri == null)
				uri = "";
			else
				uri += "&";
			uri += StringTools.urlEncode(p.name) + "=" + StringTools.urlEncode(p.value);
		}
		var question = path.split("?").length <= 1;
		if (uri != null)
			path += (if (question) "?" else "&") + uri;

		var opts = {
			protocol: parsedUrl.protocol,
			hostname: host,
			port: port,
			method: post ? 'POST' : 'GET',
			path: path,
			headers: h
		};
		function httpResponse(res) {
			res.setEncoding('binary');
			var s = res.statusCode;
			if (s != null)
				onStatus(s);
			var data = [];
			res.on('data', function(chunk:String) {
				data.push(Buffer.from(chunk, 'binary'));
			});
			res.on('end', function(_) {
				var buf = (data.length == 1 ? data[0] : Buffer.concat(data));
				responseBytes = Bytes.ofData(buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength));
				req = null;

				// store response headers
				responseHeaders = new haxe.ds.StringMap();
				for (field in Reflect.fields(res.headers))
				{
					responseHeaders.set(field, Reflect.field(res.headers, field));
				}

				if (s != null && s >= 200 && s < 400) {
					success(responseBytes);
				} else {
					onError("Http Error #" + s);
				}
			});
		}
		req = secure ? js.node.Https.request(untyped opts, httpResponse) : js.node.Http.request(untyped opts, httpResponse);
		if (post)
			if (postData != null) {
				req.write(postData);
			} else if(postBytes != null) {
				req.setHeader("Content-Length", '${postBytes.length}');
				req.write(Buffer.from(postBytes.getData()));
			}

		req.end();
	}
}
#end
