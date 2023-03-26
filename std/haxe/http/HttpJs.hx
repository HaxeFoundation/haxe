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

#if js
import js.html.XMLHttpRequestResponseType;
import js.html.Blob;
import haxe.io.Bytes;

class HttpJs extends haxe.http.HttpBase {
	public var async:Bool;
	public var withCredentials:Bool;
	public var responseHeaders:Map<String, String>;

	var req:js.html.XMLHttpRequest;

	public function new(url:String) {
		async = true;
		withCredentials = false;
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
		this.responseAsString = null;
		this.responseBytes = null;
		this.responseHeaders = null;
		var r = req = js.Browser.createXMLHttpRequest();
		var onreadystatechange = function(_) {
			if (r.readyState != 4)
				return;
			var s = try r.status catch (e:Dynamic) null;
			if (s == 0 && js.Browser.supported && js.Browser.location != null) {
				// If the request is local and we have data: assume a success (jQuery approach):
				var protocol = js.Browser.location.protocol.toLowerCase();
				var rlocalProtocol = ~/^(?:about|app|app-storage|.+-extension|file|res|widget):$/;
				var isLocal = rlocalProtocol.match(protocol);
				if (isLocal) {
					s = r.response != null ? 200 : 404;
				}
			}
			if (s == js.Lib.undefined)
				s = null;
			if (s != null)
				onStatus(s);
			if (s != null && s >= 200 && s < 400) {
				req = null;

				// split headers and remove the last \r\n\r\n
				var headers = r.getAllResponseHeaders().split('\r\n');
				headers = headers.filter(h -> h != '');

				// store response headers
				responseHeaders = new haxe.ds.StringMap();
				for (hline in headers) {
					var a = hline.split(": ");
					var hname = a.shift();
					var hval = if (a.length == 1) a[0] else a.join(": ");
					hval = StringTools.ltrim(StringTools.rtrim(hval));
					responseHeaders.set(hname, hval);
				}

				success(Bytes.ofData(r.response));
			} else if (s == null || (s == 0 && r.response == null)) {
				req = null;
				onError("Failed to connect or resolve host");
			} else
				switch (s) {
					case 12029:
						req = null;
						onError("Failed to connect to host");
					case 12007:
						req = null;
						onError("Unknown host");
					default:
						req = null;
						responseBytes = r.response != null ? Bytes.ofData(r.response) : null;
						onError("Http Error #" + r.status);
				}
		};
		if (async)
			r.onreadystatechange = onreadystatechange;
		var uri:Null<Any> = switch [postData, postBytes] {
			case [null, null]: null;
			case [str, null]: str;
			case [null, bytes]: new Blob([bytes.getData()]);
			case _: null;
		}
		if (uri != null)
			post = true;
		else
			for (p in params) {
				if (uri == null)
					uri = "";
				else
					uri = uri + "&";
				uri = uri + StringTools.urlEncode(p.name) + "=" + StringTools.urlEncode(p.value);
			}
		try {
			if (post)
				r.open("POST", url, async);
			else if (uri != null) {
				var question = url.split("?").length <= 1;
				r.open("GET", url + (if (question) "?" else "&") + uri, async);
				uri = null;
			} else
				r.open("GET", url, async);
			r.responseType = ARRAYBUFFER;
		} catch (e:Dynamic) {
			req = null;
			onError(e.toString());
			return;
		}
		r.withCredentials = withCredentials;
		if (!Lambda.exists(headers, function(h) return h.name == "Content-Type") && post && postData == null)
			r.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

		for (h in headers)
			r.setRequestHeader(h.name, h.value);
		r.send(uri);
		if (!async)
			onreadystatechange(null);
	}

	/**
		Makes a synchronous request to `url`.

		This creates a new Http instance and makes a GET request by calling its
		`request(false)` method.

		If `url` is null, the result is unspecified.
	**/
	public static function requestUrl(url:String):String {
		var h = new Http(url);
		h.async = false;
		var r = null;
		h.onData = function(d) {
			r = d;
		}
		h.onError = function(e) {
			throw e;
		}
		h.request(false);
		return r;
	}
}
#end
