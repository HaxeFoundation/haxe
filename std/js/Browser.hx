/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package js;

import js.html.Storage;
import js.html.XMLHttpRequest;

class Browser {
	public static var window(get, never):js.html.DOMWindow;
	inline static function get_window() return untyped __js__("window");

	public static var document(get, never):js.html.Document;
	inline static function get_document() return untyped __js__("window.document");

	public static var location(get, never):js.html.Location;
	inline static function get_location() return untyped __js__("window.location");

	public static var navigator(get, never):js.html.Navigator;
	inline static function get_navigator() return untyped __js__("window.navigator");
	
	/**
	 * Safely gets the browser's local storage, or returns null if localStorage is unsupported or
	 * disabled.
	 */
	public static function getLocalStorage() : Storage
	{
		try {
			var s = window.localStorage;
			s.getItem("");
			return s;
		} catch( e : Dynamic ) {
			return null;
		}
	}

	/**
	 * Safely gets the browser's session storage, or returns null if sessionStorage is unsupported
	 * or disabled.
	 */
	public static function getSessionStorage() : Storage
	{
		try {
			var s = window.sessionStorage;
			s.getItem("");
			return s;
		} catch( e : Dynamic ) {
			return null;
		}
	}

	/**
	 * Creates an XMLHttpRequest, with a fallback to ActiveXObject for ancient versions of Internet
	 * Explorer.
	 */
	public static function createXMLHttpRequest() : XMLHttpRequest
	{
		if( untyped __js__("typeof XMLHttpRequest") != "undefined" ) {
			return new XMLHttpRequest();
		}
		if( untyped __js__("typeof ActiveXObject") != "undefined" ) {
			return untyped __new__("ActiveXObject","Microsoft.XMLHTTP");
		}
		throw "Unable to create XMLHttpRequest object.";
	}

}
