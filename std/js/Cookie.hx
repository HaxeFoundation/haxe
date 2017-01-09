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
package js;

class Cookie {
	/**
		Create or update a cookie.
		@param  expireDelay  In seconds. If null, the cookie expires at end of session.
	**/
	public static function set( name : String, value : String, ?expireDelay : Int, ?path : String, ?domain : String ){
		var s = name+"="+StringTools.urlEncode(value);
		if( expireDelay != null ){
			var d = DateTools.delta(Date.now(),expireDelay*1000);
			s += ";expires=" + untyped d.toGMTString();
		}
		if( path != null ){
			s += ";path="+path;
		}
		if( domain != null ){
			s += ";domain="+domain;
		}
		Browser.document.cookie = s;
	}

	/**
		Returns all cookies.
	**/
	public static function all(){
		var h = new haxe.ds.StringMap();
		var a = Browser.document.cookie.split(";");
		for( e in a ){
			e = StringTools.ltrim(e);
			var t = e.split("=");
			if( t.length < 2 )
				continue;
			h.set(t[0],StringTools.urlDecode(t[1]));
		}
		return h;
	}

	/**
		Returns value of a cookie.
	**/
	public static function get( name : String ){
		return all().get(name);
	}

	/**
		Returns true if a cookie `name` exists.
	**/
	public static function exists( name : String ){
		return all().exists(name);
	}

	/**
		Remove a cookie.
	**/
	public static function remove( name : String, ?path : String, ?domain : String ){
		set(name,"",-10,path,domain);
	}
}
