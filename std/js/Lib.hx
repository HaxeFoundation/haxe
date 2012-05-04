/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package js;

import js.Dom;

class Lib {

	public static var document : Document;
	public static var window : Window;
	static var onerror : String -> Array<String> -> Bool = null;

	/**
		Inserts a 'debugger' statement that will make a breakpoint if a debugger is available.
	**/
	public static inline function debug() {
		untyped __js__("debugger");
	}

	/**
		Display an alert message box containing the given message
	**/
	public static function alert( v : Dynamic ) {
		untyped __js__("alert")(js.Boot.__string_rec(v,""));
	}

	public static inline function eval( code : String ) : Dynamic {
		return untyped __js__("eval")(code);
	}

	public static inline function setErrorHandler( f ) {
		onerror = f;
	}

	static function __init__() {
		if( untyped __js__("typeof document") != "undefined" )
			document = untyped __js__("document");
		if( untyped __js__("typeof window") != "undefined" ) {
			window = untyped __js__("window");
			window.onerror = function( msg, url, line ) {
				var f = Lib.onerror;
				if( f == null )
					return false;
				return f(msg, [url+":"+line]);
			};
		}
	}

}
