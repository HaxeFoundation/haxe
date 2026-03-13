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

package js.node.url;

/**
	Browser-compatible URL class, implemented by following the WHATWG URL Standard.
	[Examples of parsed URLs](https://url.spec.whatwg.org/#example-url-parsing) may be found in the Standard itself.
**/
@:jsRequire("url", "URL")
extern class URL {
	/**
		Creates a new `URL` object by parsing the `input` relative to the `base`.
		If `base` is passed as a string, it will be parsed equivalent to `new URL(base)`.
	**/
	@:overload(function(input:String, ?base:URL):Void {})
	function new(input:String, ?base:String):Void;

	/**
		Gets and sets the fragment portion of the URL.
	**/
	var hash:String;

	/**
		Gets and sets the host portion of the URL.
	**/
	var host:String;

	/**
		Gets and sets the hostname portion of the URL
		The key difference between `url.host` and `url.hostname` is that `url.hostname` does not include the port.
	**/
	var hostname:String;

	/**
		Gets and sets the serialized URL.
	**/
	var href:String;

	/**
		Gets the read-only serialization of the URL's origin.
	**/
	var origin(default, null):String;

	/**
		Gets and sets the password portion of the URL.
	**/
	var password:String;

	/**
		Gets and sets the path portion of the URL.
	**/
	var pathname:String;

	/**
		Gets and sets the port portion of the URL.

		The port value may be a number or a string containing a number in the range `0` to `65535` (inclusive).
		Setting the value to the default port of the `URL` objects given `protocol` will result in the port value becoming the empty string (`''`).
	**/
	var port:String;

	/**
		Gets and sets the protocol portion of the URL.
	**/
	var protocol:String;

	/**
		Gets and sets the serialized query portion of the URL.
	**/
	var search:String;

	/**
		Gets the `URLSearchParams` object representing the query parameters of the URL.
		This property is read-only; to replace the entirety of query parameters of the URL, use the `url.search` setter.
		See [URLSearchParams](https://nodejs.org/api/url.html#url_class_urlsearchparams) documentation for details.
	**/
	var searchParams(default, null):URLSearchParams;

	/**
		Gets and sets the username portion of the URL.
	**/
	var username:String;

	/**
		The `toString()` method on the `URL` object returns the serialized URL.
		The value returned is equivalent to that of `url.href` and `url.toJSON()`.

		Because of the need for standard compliance, this method does not allow users to customize the serialization process of the URL.
		For more flexibility, `require('url').format()` method might be of interest.
	**/
	function toString():String;

	/**
		The `toJSON()` method on the `URL` object returns the serialized URL.
		The value returned is equivalent to that of `url.href` and `url.toString()`.

		This method is automatically called when an `URL` object is serialized with `JSON.stringify()`.
	**/
	function toJSON():String;
}
