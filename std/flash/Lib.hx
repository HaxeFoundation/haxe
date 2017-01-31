/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	Platform-specific Flash Library. Provides some platform-specific 
	functions for the Flash target.
**/
class Lib {

	public static var current : flash.display.MovieClip;

	public inline static function getTimer() : Int {
		return untyped __global__["flash.utils.getTimer"]();
	}

	public static function eval( path : String ) : Dynamic {
		var p = path.split(".");
		var fields = new Array();
		var o : Dynamic = null;
		while( p.length > 0 ) {
			try {
				o = untyped __global__["flash.utils.getDefinitionByName"](p.join("."));
			} catch( e : Dynamic ) {
				fields.unshift(p.pop());
			}
			if( o != null )
				break;
		}
		for( f in fields ) {
			if( o == null ) return null;
			o = untyped o[f];
		}
		return o;
	}

	public static function getURL( url : flash.net.URLRequest, ?target : String ) {
		var f = untyped __global__["flash.net.navigateToURL"];
		if( target == null )
			f(url);
		else
			(cast f)(url,target);
	}

	public static function fscommand( cmd : String, ?param : String ) {
		untyped __global__["flash.system.fscommand"](cmd,if( param == null ) "" else param);
	}

	public static function trace( arg : Dynamic ) {
		untyped __global__["trace"](arg);
	}
	
	public static function describeType( value : Dynamic ) : flash.xml.XML {
		return untyped __global__["flash.utils.describeType"](value);
	}

	public static function attach( name : String ) : flash.display.MovieClip {
		var cl = untyped __as__(__global__["flash.utils.getDefinitionByName"](name),Class);
		return untyped __new__(cl);
	}

	public inline static function as<T>( v : Dynamic, c : Class<T> ) : Null<T> {
		return untyped __as__(v,c);
	}

	public static function redirectTraces() {
		if (flash.external.ExternalInterface.available)
			haxe.Log.trace = traceToConsole;
	}

	static function traceToConsole(v : Dynamic, ?inf : haxe.PosInfos ) {
		var type = if( inf != null && inf.customParams != null ) inf.customParams[0] else null;
		if( type != "warn" && type != "info" && type != "debug" && type != "error" )
			type = if( inf == null ) "error" else "log";
		var str = if( inf == null ) "" else inf.fileName + ":" + inf.lineNumber + " : ";
		try	str += Std.string(v) catch( e : Dynamic ) str += "????";
		str = str.split("\\").join("\\\\");
		flash.external.ExternalInterface.call("console."+type,str);
	}
}


