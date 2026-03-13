/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.https;

/**
	An Agent object for HTTPS similar to `http.Agent`.
	See [https.request](https://nodejs.org/api/http.html#http_http_request_options_callback) for more information.
**/
@:jsRequire("https", "Agent")
extern class Agent extends js.node.http.Agent {
	function new(?options:HttpsAgentOptions);
}

typedef HttpsAgentOptions = {
	> js.node.http.Agent.HttpAgentOptions,

	/**
		maximum number of TLS cached sessions. Use `0` to disable TLS session caching.

		Default: `100`.
	**/
	@:optional var maxCachedSessions:Int;

	/**
		the value of [Server Name Indication extension](https://en.wikipedia.org/wiki/Server_Name_Indication) to be sent to the server.
		Use empty string `''` to disable sending the extension.

		Default: hostname of the target server, unless the target server is specified using an IP address, in which case the default is `''` (no extension).
	**/
	@:optional var servername:String;
}
