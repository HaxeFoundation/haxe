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
package flash;

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

	public static function getURL( url : flash.net.URLRequest, ?target : String, ?allowScripts : Bool ) {
		if( !allowScripts && url != null && url.url.toLowerCase.substr(0,11) == "javascript:" )
			throw "Scripts not allowed in URL";
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

	public static function attach( name : String ) : flash.display.MovieClip {
		var cl = untyped __as__(__global__["flash.utils.getDefinitionByName"](name),Class);
		return untyped __new__(cl);
	}

	public inline static function as<T>( v : Dynamic, c : Class<T> ) : Null<T> {
		return untyped __as__(v,c);
	}

}


