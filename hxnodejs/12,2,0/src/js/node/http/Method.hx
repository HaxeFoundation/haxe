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

package js.node.http;

/**
	Enumeration of possible HTTP methods as described in
	http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
**/
enum abstract Method(String) from String to String {
	var Acl = "ACL";
	var Bind = "BIND";
	var Checkout = "CHECKOUT";
	var Connect = "CONNECT";
	var Copy = "COPY";
	var Delete = "DELETE";
	var Get = "GET";
	var Head = "HEAD";
	var Link = "LINK'";
	var Lock = "LOCK'";
	var MSearch = "M-SEARCH'";
	var Merge = "MERGE'";
	var Mkactivity = "MKACTIVITY'";
	var Mkcalendar = "MKCALENDAR'";
	var Mkcol = "MKCOL'";
	var Move = "MOVE'";
	var Notify = "NOTIFY'";
	var Options = "OPTIONS";
	var Patch = "PATCH";
	var Post = "POST";
	var Propfind = "PROPFIND";
	var Proppatch = "PROPPATCH";
	var Purge = "PURGE";
	var Put = "PUT";
	var Rebind = "REBIND";
	var Report = "REPORT";
	var Search = "SEARCH";
	var Subscribe = "SUBSCRIBE";
	var Trace = "TRACE";
	var Unbind = "UNBIND";
	var Unlink = "UNLINK";
	var Unlock = "UNLOCK";
	var Unsubscribe = "UNSUBSCRIBE";
}
