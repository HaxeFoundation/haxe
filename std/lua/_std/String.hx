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
@:coreApi extern class String {
	var length(default,null) : Int;

	function new(string:String) : Void;
	function toUpperCase() : String;
	function toLowerCase() : String;
	function charAt( index : Int) : String;
	function indexOf( str : String, ?startIndex : Int ) : Int;
	function lastIndexOf( str : String, ?startIndex : Int ) : Int;
	function split( delimiter : String ) : Array<String>;
	function toString() : String;
	function substring( startIndex : Int, ?endIndex : Int ) : String;

	inline function charCodeAt( index : Int) : Null<Int> {
		return untyped HxOverrides.cca(this, index);
	}

	inline function substr( pos : Int, ?len : Int ) : String {
		return untyped HxOverrides.substr(this, pos, len);
	}

	static function fromCharCode( code : Int ) : String;
}
