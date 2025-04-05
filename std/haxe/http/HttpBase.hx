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

import haxe.io.Bytes;

private typedef StringKeyValue = {
	var name:String;
	var value:String;
}

/**
	This class can be used to handle Http requests consistently across
	platforms. There are two intended usages:

	- call `haxe.Http.requestUrl(url)` and receive the result as a `String`
	(only available on `sys` targets)
	- create a `new haxe.Http(url)`, register your callbacks for `onData`,
	`onError` and `onStatus`, then call `request()`.
**/
class HttpBase {
	/**
		The url of `this` request. It is used only by the `request()` method and
		can be changed in order to send the same request to different target
		Urls.
	**/
	public var url:String;

	public var responseData(get,never):Null<String>;
	public var responseBytes(default,null):Null<Bytes>;

	var responseAsString:Null<String>;
	var postData:Null<String>;
	var postBytes:Null<Bytes>;
	var headers:Array<StringKeyValue>;
	var params:Array<StringKeyValue>;

	final emptyOnData:(String)->Void;

	/**
		Creates a new Http instance with `url` as parameter.

		This does not do a request until `request()` is called.

		If `url` is null, the field url must be set to a value before making the
		call to `request()`, or the result is unspecified.

		(Php) Https (SSL) connections are allowed only if the OpenSSL extension
		is enabled.
	**/
	public function new(url:String) {
		this.url = url;
		headers = [];
		params = [];
		emptyOnData = onData;
	}

	/**
		Sets the header identified as `name` to value `value`.

		If `name` or `value` are null, the result is unspecified.

		This method provides a fluent interface.
	**/
	public function setHeader(name:String, value:String) {
		for (i in 0...headers.length) {
			if (headers[i].name == name) {
				headers[i] = {name: name, value: value};
				return #if hx3compat this #end;
			}
		}
		headers.push({name: name, value: value});
		#if hx3compat
		return this;
		#end
	}

	public function addHeader(header:String, value:String) {
		headers.push({name: header, value: value});
		#if hx3compat
		return this;
		#end
	}

	/**
		Sets the parameter identified as `name` to value `value`.

		If `name` or `value` are null, the result is unspecified.

		This method provides a fluent interface.
	**/
	public function setParameter(name:String, value:String) {
		for (i in 0...params.length) {
			if (params[i].name == name) {
				params[i] = {name: name, value: value};
				return #if hx3compat this #end;
			}
		}
		params.push({name: name, value: value});
		#if hx3compat
		return this;
		#end
	}

	public function addParameter(name:String, value:String) {
		params.push({name: name, value: value});
		#if hx3compat
		return this;
		#end
	}

	/**
		Sets the post data of `this` Http request to `data` string.

		There can only be one post data per request. Subsequent calls to
		this method or to `setPostBytes()` overwrite the previously set value.

		If `data` is null, the post data is considered to be absent.

		This method provides a fluent interface.
	**/
	public function setPostData(data:Null<String>) {
		postData = data;
		postBytes = null;
		#if hx3compat
		return this;
		#end
	}

	/**
		Sets the post data of `this` Http request to `data` bytes.

		There can only be one post data per request. Subsequent calls to
		this method or to `setPostData()` overwrite the previously set value.

		If `data` is null, the post data is considered to be absent.

		This method provides a fluent interface.
	**/
	public function setPostBytes(data:Null<Bytes>) {
		postBytes = data;
		postData = null;
		#if hx3compat
		return this;
		#end
	}

	/**
		Sends `this` Http request to the Url specified by `this.url`.

		If `post` is true, the request is sent as POST request, otherwise it is
		sent as GET request.

		Depending on the outcome of the request, this method calls the
		`onStatus()`, `onError()`, `onData()` or `onBytes()` callback functions.

		If `this.url` is null, the result is unspecified.

		If `this.url` is an invalid or inaccessible Url, the `onError()` callback
		function is called.

		[js] If `this.async` is false, the callback functions are called before
		this method returns.
	**/
	public function request(?post:Bool):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	/**
		This method is called upon a successful request, with `data` containing
		the result String.

		The intended usage is to bind it to a custom function:
		`httpInstance.onData = function(data) { // handle result }`
	**/
	public dynamic function onData(data:String) {}

	/**
		This method is called upon a successful request, with `data` containing
		the result String.

		The intended usage is to bind it to a custom function:
		`httpInstance.onBytes = function(data) { // handle result }`
	**/
	public dynamic function onBytes(data:Bytes) {}

	/**
		This method is called upon a request error, with `msg` containing the
		error description.

		The intended usage is to bind it to a custom function:
		`httpInstance.onError = function(msg) { // handle error }`
	**/
	public dynamic function onError(msg:String) {}

	/**
		This method is called upon a Http status change, with `status` being the
		new status.

		The intended usage is to bind it to a custom function:
		`httpInstance.onStatus = function(status) { // handle status }`
	**/
	public dynamic function onStatus(status:Int) {}

	/**
		Override this if extending `haxe.Http` with overriding `onData`
	**/
	function hasOnData():Bool {
		return !Reflect.compareMethods(onData, emptyOnData);
	}

	function success(data:Bytes) {
		responseBytes = data;
		responseAsString = null;
		if (hasOnData()) {
			onData(responseData);
		}
		onBytes(responseBytes);
	}

	function get_responseData() {
		if (responseAsString == null && responseBytes != null) {
			#if neko
			responseAsString = neko.Lib.stringReference(responseBytes);
			#else
			responseAsString = responseBytes.getString(0, responseBytes.length, UTF8);
			#end
		}
		return responseAsString;
	}
}
