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

extern class String {
	var length(get,null) : Int;

	function new(string:String) {
		this = string;
	}

	inline function toUpperCase() : String return untyped this.upper();
	inline function toLowerCase() : String return untyped this.lower();
	inline function indexOf( str : String, ?startIndex : Int ) : Int {
		if (startIndex == null) startIndex = 1;
		else startIndex += 1;
		return lua.StringTools.find(this, str, startIndex, str.length, true);
	}
	inline function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		var i = 0;
		var ret = 0;
		while(i != null){
			i = this.indexOf(str, i);
			if (i != null) ret = i;
		}
		return ret-1;
	}
	inline function split( delimiter : String ) : Array<String> {
		return [];
	}
	inline function toString() : String {
		return this;
	}
	inline function substring( startIndex : Int, ?endIndex : Int ) : String {
		if (endIndex == null) endIndex = this.length;
		return untyped lua.StringTools.sub(this, startIndex + 1,endIndex + 1);
	}

	inline function get_length() : Int {
		return lua.StringTools.len(this);
	}
	inline function charAt( index : Int) : String {
		return lua.StringTools.sub(this,index+1, index+1);
	}
	inline function charCodeAt( index : Int) : Null<Int> {
		return lua.StringTools.byte(this,index+1);
	}

	inline function substr( pos : Int, ?len : Int ) : String {
		if (len == null || len > pos + this.length) len = this.length;
		return lua.StringTools.sub(this, pos + 1, pos+len + 1);
	}

	static function fromCharCode( code : Int ) : String;
}

