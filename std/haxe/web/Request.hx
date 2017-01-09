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
package haxe.web;

#if (!neko && !php && !js)
#error "Not supported on this target"
#end

class Request {

	/**
		Returns the current page GET and POST parameters (only GET parameters for Javascript)
	**/
	public static function getParams() : Map<String,String> {
		#if neko
		return neko.Web.getParams();
		#elseif php
		return php.Web.getParams();
		#elseif js
		var get : String = js.Browser.location.search.substr(1);
		var params = new haxe.ds.StringMap();
		for( p in ~/[&;]/g.split(get) ) {
			var pl = p.split("=");
			if( pl.length < 2 ) continue;
			var name = pl.shift();
			params.set(StringTools.urlDecode(name), StringTools.urlDecode(pl.join("=")));
		}
		return params;
		#end
	}

	/**
		Returns the local server host name
	**/
	public static function getHostName() : String {
		#if neko
		return neko.Web.getHostName();
		#elseif php
		return php.Web.getHostName();
		#elseif js
		return js.Browser.location.host; // includes port
		#end
	}

	/**
		Returns the original request URL (before any server internal redirections)
	**/
	public static function getURI() : String {
		#if neko
		return neko.Web.getURI();
		#elseif php
		return php.Web.getURI();
		#elseif js
		return js.Browser.location.pathname;
		#end
	}

}
