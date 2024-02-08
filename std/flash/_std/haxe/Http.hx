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

package haxe;

import haxe.io.Bytes;

typedef Http = HttpFlash;

class HttpFlash extends haxe.http.HttpBase {
	var req:flash.net.URLLoader;

	/**
		Cancels `this` Http request if `request` has been called and a response
		has not yet been received.
	**/
	public function cancel() {
		if (req == null)
			return;
		req.close();
		req = null;
	}

	public override function request(?post:Bool) {
		responseAsString = null;
		responseBytes = null;
		var loader = req = new flash.net.URLLoader();
		loader.dataFormat = BINARY;
		loader.addEventListener("complete", function(e) {
			req = null;
			success(Bytes.ofData(loader.data));
		});
		loader.addEventListener("httpStatus", function(e:flash.events.HTTPStatusEvent) {
			// on Firefox 1.5, Flash calls onHTTPStatus with 0 (!??)
			if (e.status != 0)
				onStatus(e.status);
		});
		loader.addEventListener("ioError", function(e:flash.events.IOErrorEvent) {
			req = null;
			responseBytes = Bytes.ofData(loader.data);
			onError(e.text);
		});
		loader.addEventListener("securityError", function(e:flash.events.SecurityErrorEvent) {
			req = null;
			onError(e.text);
		});

		// headers
		var param = false;
		var vars = new flash.net.URLVariables();
		for (p in params) {
			param = true;
			Reflect.setField(vars, p.name, p.value);
		}
		var small_url = url;
		if (param && !post) {
			var k = url.split("?");
			if (k.length > 1) {
				small_url = k.shift();
				vars.decode(k.join("?"));
			}
		}
		// Bug in flash player 9 ???
		small_url.split("xxx");

		var request = new flash.net.URLRequest(small_url);
		for (h in headers)
			request.requestHeaders.push(new flash.net.URLRequestHeader(h.name, h.value));

		if (postData != null) {
			request.data = postData;
			request.method = "POST";
		} else if (postBytes != null) {
			request.data = postBytes.getData();
			request.method = "POST";
		} else {
			request.data = vars;
			request.method = if (post) "POST" else "GET";
		}

		try {
			loader.load(request);
		} catch (e:Dynamic) {
			req = null;
			onError("Exception: " + Std.string(e));
		}
	}
}
