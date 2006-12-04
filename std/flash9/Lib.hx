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

	public static function getTimer() : Float {
		return untyped __global__["flash.utils.getTimer"]();
	}

	public static function eval( path : String ) : Dynamic {
		var p = path.split(".");
		var fields = new Array();
		var o = null;
		while( p.length > 0 ) {
			try {
				o = untyped __global__["flash.utils.getDefinitionByName"](p.join("."));
			} catch( e : Dynamic ) {
				fields.unshift(p.pop());
			}
			if( o != null )
				break;
		}
		if( o == null )
			return null;
		for( f in fields )
			o = untyped o[f];
		return o;
	}

	public static function getURL( url : String, ?target : String ) {
		var f = untyped __global__["flash.net.navigateToURL"];
		if( target == null )
			f(url);
		else
			(cast f)(url,target);
	}

	public static function fscommand( cmd : String, ?param : Dynamic ) {
		untyped __global__["flash.system.fscommand"](cmd,param);
	}

}


