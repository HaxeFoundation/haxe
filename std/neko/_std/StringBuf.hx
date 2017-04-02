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
@:coreApi class StringBuf {

	private var b : Dynamic;

	public var length(get,never) : Int;

	public function new() : Void {
		b = __make();
	}

	function get_length() : Int {
		return __get_length == null ? untyped __dollar__ssize( __to_string(b) ) : __get_length(b);
	}

	public inline function add<T>( x : T ) : Void {
		__add(b,x);
	}

	public inline function addSub( s : String, pos : Int, ?len : Int ) : Void {
		__add_sub(b,untyped s.__s,pos,len == null ? s.length - pos : len);
	}

	public inline function addChar( c : Int ) : Void untyped {
		__add_char(b,c);
	}

	public inline function toString() : String {
		return new String(__to_string(b));
	}

	static var __make : Dynamic = neko.Lib.load("std","buffer_new",0);
	static var __add : Dynamic = neko.Lib.load("std","buffer_add",2);
	static var __add_char : Dynamic = neko.Lib.load("std","buffer_add_char",2);
	static var __add_sub : Dynamic = neko.Lib.load("std","buffer_add_sub",4);
	static var __to_string : Dynamic = neko.Lib.load("std","buffer_string",1);
	static var __get_length : Dynamic = try neko.Lib.load("std","buffer_get_length",1) catch( e : Dynamic ) null;

}
