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
package flash;

class Lib {

	public static var _global : Dynamic;
	public static var _root : MovieClip;
	public static var current : MovieClip;
	static var onerror : String -> Array<String> -> Void;

	public static function trace( str : String ) {
		untyped __trace__(str);
	}

	public static function eval( str : String ) : Dynamic {
		return untyped __eval__(str);
	}

	public static function getURL( url : String, ?target : String ) {
		untyped __geturl__(url,if( target == null ) "_self" else target);
	}

	public static function fscommand( cmd : String, ?param : Dynamic ) {
		untyped __geturl__("FSCommand:"+cmd,if( param == null ) "" else param);
	}

	public static function print( cmd : String, ?kind : String ) {
		kind = if (kind == "bframe" || kind == "bmax") "print:#"+kind else "print:";
		untyped __geturl__(kind,cmd);
	}

	public inline static function getTimer() : Int {
		return untyped __gettimer__();
	}

	public static function getVersion() : String {
		return untyped _root["$version"];
	}

	public static function registerClass( name : String, cl : {} ) {
		untyped _global["Object"]["registerClass"](name,cl);
	}

	public static function keys( v : Dynamic ) : Array<String> {
		return untyped __keys__(v);
	}

	public static function setErrorHandler(f) {
		onerror = f;
	}

}


