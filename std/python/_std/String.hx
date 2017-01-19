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
#if !macro
import python.internal.StringImpl;
#end
@:coreApi
@:native("str")
extern class String {
	var length(default,null) : Int;
	
	function new(string:String) : Void;
	
	@:runtime public inline function toUpperCase() : String {
		return StringImpl.toUpperCase(this);
	}

	@:runtime public inline function toLowerCase() : String {
		return StringImpl.toLowerCase(this);
	}

	inline public function charAt(index : Int) : String
	{
		return StringImpl.charAt(this, index);
	}

	inline public function charCodeAt( index : Int) : Null<Int>
	{
		return StringImpl.charCodeAt(this, index);
	}

	inline function indexOf( str : String, ?startIndex : Int ) : Int {
		return StringImpl.indexOf(this, str, startIndex);
	}

	inline function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		return StringImpl.lastIndexOf(this, str, startIndex);
	}

	inline function split( delimiter : String ) : Array<String> {
		return StringImpl.split(this, delimiter);
	}

	inline public function substr( pos : Int, ?len : Int ) : String
	{
		return StringImpl.substr(this, pos, len);
	}

	inline function substring( startIndex : Int, ?endIndex : Int ) : String {
		return StringImpl.substring(this, startIndex, endIndex);
	}

	inline function toString() : String return StringImpl.toString(this);

	public static inline function fromCharCode( code : Int ) : String {
		return StringImpl.fromCharCode(code);
	}
}
