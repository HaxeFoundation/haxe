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

import lua.Lua;
import lua.Table;
import lua.Boot;

@:coreApi
class String {
	public var length(default,null) : Int;


	public function new(string:String) untyped {
		if (string != null) __lua__("self = string");
		else __lua__("self = ''");
	}

	static function __init__() : Void untyped{
		__lua__("getmetatable('').__index = String.__index;");
		__lua__("getmetatable('').__add = function(a,b) return Std.string(a)..Std.string(b) end;");
	}

	@:keep
	static function __index(s:Dynamic, k:Dynamic) : Dynamic {
		if (k == "length") return untyped __lua__("#s");
		else return untyped String.prototype[k];
	}


	public function toUpperCase() : String return lua.StringTools.upper(this);
	public function toLowerCase() : String return lua.StringTools.lower(this);
	public function indexOf( str : String, ?startIndex : Int ) : Int {
		if (startIndex == null) startIndex = 1;
		else startIndex += 1;
		var r = lua.StringTools.find(this, str, startIndex, true);
		return untyped r && (r - 1) || (-1);
	}

	public function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		var i = 0;
		var ret = -1;
		if( startIndex == null ) startIndex = length;
		while( true ) {
			var p = indexOf(str, ret+1);
			if( p == -1 || p > startIndex ) return ret;
			ret = p;
		}
	}

	public function split( delimiter : String ) : Array<String> {
		var captures = (delimiter != "" ) ?
			  "(.-)(" + Boot.patternQuote(delimiter) + ")"
			: "(.)";
		return Boot.luaIteratorToArray(lua.StringTools.gmatch(this, captures));
	}

	public function toString() : String {
		return this;
	}
	public function substring( startIndex : Int, ?endIndex : Int ) : String {
		if (endIndex == null) endIndex = this.length;
		return untyped lua.StringTools.sub(this, startIndex + 1,endIndex + 1);
	}

	function get_length() : Int {
		return lua.StringTools.len(this);
	}
	public function charAt( index : Int) : String {
		return lua.StringTools.sub(this,index+1, index+1);
	}
	public function charCodeAt( index : Int) : Null<Int> {
		return lua.StringTools.byte(this,index+1);
	}

	public function substr( pos : Int, ?len : Int ) : String {
		if (len == null || len > pos + this.length) len = this.length;
		else if (len < 0) len = length + len;
		if (pos < 0) pos = length + pos;
		if (pos < 0) pos = 0;
		return lua.StringTools.sub(this, pos + 1, pos+len);
	}

	public inline static function fromCharCode( code : Int ) : String {
		return lua.StringTools.char(code);
	}
}

