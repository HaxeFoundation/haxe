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

package js;

import js.html.Storage;
import js.html.XMLHttpRequest;

class Browser {
	/** The global scope typed with fields available only in a worker context. */
	public static var self(get, never):js.html.WorkerGlobalScope;

	static inline function get_self():js.html.WorkerGlobalScope {
		return js.Lib.global;
	}

	/** The global window object. */
	public static var window(get, never):js.html.Window;

	extern inline static function get_window()
		return js.Syntax.code("window");

	/** Shortcut to Window.document. */
	public static var document(get, never):js.html.HTMLDocument;

	extern inline static function get_document()
		return window.document;

	/** Shortcut to Window.location. */
	public static var location(get, never):js.html.Location;

	extern inline static function get_location()
		return js.Lib.global.location;

	/** Shortcut to Window.navigator. */
	public static var navigator(get, never):js.html.Navigator;

	extern inline static function get_navigator()
		return js.Lib.global.navigator;

	/** Shortcut to Window.console. */
	public static var console(get, never):js.html.ConsoleInstance;

	extern inline static function get_console()
		return js.Lib.global.console;

	/**
	 * True if a window object exists, false otherwise.
	 *
	 * This can be used to check if the code is being executed in a non-browser
	 * environment such as node.js.
	 */
	public static var supported(get, never):Bool;

	static function get_supported()
		return
			js.Syntax.typeof(window) != "undefined" &&
			js.Syntax.typeof(window.location) != "undefined" &&
			js.Syntax.typeof(window.location.protocol) == "string";
	/**
	 * Safely gets the browser's local storage, or returns null if localStorage is unsupported or
	 * disabled.
	 */
	public static function getLocalStorage():Null<Storage> {
		try {
			var s = window.localStorage;
			s.getItem("");
			if (s.length == 0) {
				var key = "_hx_" + Math.random();
				s.setItem(key, key);
				s.removeItem(key);
			}
			return s;
		} catch (e:Dynamic) {
			return null;
		}
	}

	/**
	 * Safely gets the browser's session storage, or returns null if sessionStorage is unsupported
	 * or disabled.
	 */
	public static function getSessionStorage():Storage {
		try {
			var s = window.sessionStorage;
			s.getItem("");
			if (s.length == 0) {
				var key = "_hx_" + Math.random();
				s.setItem(key, key);
				s.removeItem(key);
			}
			return s;
		} catch (e:Dynamic) {
			return null;
		}
	}

	/**
	 * Creates an XMLHttpRequest, with a fallback to ActiveXObject for ancient versions of Internet
	 * Explorer.
	 */
	public static function createXMLHttpRequest():XMLHttpRequest {
		if (js.Syntax.code("typeof XMLHttpRequest") != "undefined") {
			return new XMLHttpRequest();
		}
		if (js.Syntax.code("typeof ActiveXObject") != "undefined") {
			return js.Syntax.construct("ActiveXObject", "Microsoft.XMLHTTP");
		}
		throw "Unable to create XMLHttpRequest object.";
	}

	/**
		Display an alert message box containing the given message. See also `Window.alert()`.
	**/
	public static inline function alert(v:Dynamic) {
		window.alert(Std.string(v));
	}
}
