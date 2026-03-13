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

package js.node;

import haxe.DynamicAccess;
import haxe.extern.EitherType;

/**
	The `querystring` module provides utilities for parsing and formatting URL query strings.

	@see https://nodejs.org/api/querystring.html#querystring_query_string
**/
@:jsRequire("querystring")
extern class Querystring {
	/**
		The `querystring.decode()` function is an alias for `querystring.parse()`.

		@see https://nodejs.org/api/querystring.html#querystring_querystring_decode
	**/
	@:overload(function(str:String):QuerystringParseResult {})
	static function decode(str:String, ?sep:String, ?eq:String, ?options:QuerystringParseOptions):QuerystringParseResult;

	/**
		The `querystring.encode()` function is an alias for `querystring.stringify()`.

		@see https://nodejs.org/api/querystring.html#querystring_querystring_decode
	**/
	@:overload(function(obj:{}):String {})
	static function encode(obj:{}, ?sep:String, ?eq:String, ?options:QuerystringStringifyOptions):String;

	/**
		The `querystring.escape()` method performs URL percent-encoding on the given `str` in a manner that is optimized for the specific requirements of URL query strings.

		@see https://nodejs.org/api/querystring.html#querystring_querystring_escape_str
	**/
	static dynamic function escape(str:String):String;

	/**
		The `querystring.parse()` method parses a URL query string (`str`) into a collection of key and value pairs.

		@see https://nodejs.org/api/querystring.html#querystring_querystring_parse_str_sep_eq_options
	**/
	@:overload(function(str:String):QuerystringParseResult {})
	static function parse(str:String, ?sep:String, ?eq:String, ?options:QuerystringParseOptions):QuerystringParseResult;

	/**
		The `querystring.stringify()` method produces a URL query string from a given `obj` by iterating through the object's "own properties".

		@see https://nodejs.org/api/querystring.html#querystring_querystring_stringify_obj_sep_eq_options
	**/
	@:overload(function(obj:{}):String {})
	static function stringify(obj:{}, ?sep:String, ?eq:String, ?options:QuerystringStringifyOptions):String;

	/**
		The `querystring.unescape()` method performs decoding of URL percent-encoded characters on the given `str`.

		@see https://nodejs.org/api/querystring.html#querystring_querystring_unescape_str
	**/
	static dynamic function unescape(str:String):Dynamic;
}

/**
	Options used for `Querystring.parse` method.

	@see https://nodejs.org/api/querystring.html#querystring_querystring_parse_str_sep_eq_options
**/
typedef QuerystringParseOptions = {
	/**
		The function to use when decoding percent-encoded characters in the query string. Default: `querystring.unescape()`.
	**/
	@:optional var decodeURIComponent:String->String;

	/**
		Specifies the maximum number of keys to parse. Specify `0` to remove key counting limitations. Default: `1000`.
	**/
	@:optional var maxKeys:Int;
}

/**
	The result type of `Querystring.parse`. Is a collection of either strings or array of strings.

	The object returned by the `querystring.parse()` method does not prototypically inherit from the JavaScript `Object`.
	This means that typical `Object` methods such as `obj.toString()`, `obj.hasOwnProperty()`, and others are not defined and will not work.

	@see https://nodejs.org/api/querystring.html#querystring_querystring_parse_str_sep_eq_options
**/
typedef QuerystringParseResult = DynamicAccess<EitherType<String, Array<String>>>;

/**
	Options for `Querystring.stringify` method.

	@see https://nodejs.org/api/querystring.html#querystring_querystring_stringify_obj_sep_eq_options
**/
typedef QuerystringStringifyOptions = {
	/**
		The function to use when converting URL-unsafe characters to percent-encoding in the query string. Default: `querystring.escape()`.
	**/
	@:optional var encodeURIComponent:String->String;
}
