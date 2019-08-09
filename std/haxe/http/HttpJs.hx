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
class HttpJs extends haxe.http.HttpBase {
	public var async:Bool;
	public var withCredentials:Bool;

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
		responseData = null;
		var r = req = js.Browser.createXMLHttpRequest();
		var onreadystatechange = function(_) {
			if (r.readyState != 4)
				return;
			var s = try r.status catch (e:Dynamic) null;
			if (s == 0 && js.Browser.supported) {
				// If the request is local and we have data: assume a success (jQuery approach):
				var protocol = js.Browser.location.protocol.toLowerCase();
				var rlocalProtocol = ~/^(?:about|app|app-storage|.+-extension|file|res|widget):$/;
				var isLocal = rlocalProtocol.match(protocol);
				if (isLocal) {
					s = r.responseText != null ? 200 : 404;
				}
			}
			if (s == js.Lib.undefined)
				s = null;
			if (s != null)
				onStatus(s);
			if (s != null && s >= 200 && s < 400) {
				req = null;
				onData(responseData = r.responseText);
			} else if (s == null) {
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
						responseData = r.responseText;
						onError("Http Error #" + r.status);
				}
		};
		if (async)
			r.onreadystatechange = onreadystatechange;
		var uri = postData;
		if (uri != null)
			post = true;
		else
			for (p in params) {
				if (uri == null)
					uri = "";
				else
					uri += "&";
				uri += StringTools.urlEncode(p.name) + "=" + StringTools.urlEncode(p.value);
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
