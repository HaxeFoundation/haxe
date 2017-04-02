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

package lua;

import haxe.Constraints.Function;
import lua.Table;
#if lua_jit
@:luaRequire("ffi")
extern class Ffi {

	// Declaring and accessing external symbols
	public static var C : Dynamic;
	public static function gc(cdata : Dynamic, finalizer : Function) : Void;
	public static function load (name : String, ?global : Bool) : Dynamic;
	public static function metatype<T>(ct : Ctype<T>, metatable : Table<Dynamic,Dynamic>) : Ctype<T>;
	public static function typeof(str:String) : Ctype<Dynamic>;

	// C Type functionality
	public static function alignof<T>(ct : Ctype<T>) : Int;
	public static function istype<T>(ct : Ctype<T>, obj:Dynamic) : Bool;
	public static function offsetof<T>(ct : Ctype<T>, field:String) : Int;
	public static function sizeof<T>(ct : Ctype<T>, ?nelem : Int) : Int;


	// Utility functionality
	public static function errno(?newerr : Int) : Int;
	public static function fill(dst : Dynamic, len : Int, c:Int) : Void;
	public static function string(ptr : Dynamic, ?len : Int) : String;

	@:overload(   function     (dst : Dynamic, str : String)             : Dynamic {})
	public static function copy(dst : Dynamic, src : Dynamic, len : Int) : String;

	// Target specific functionality
	public static var os : String;
	public static var arch : String;
	public static function abi(param : String) : Bool;
}

extern class Ctype<T> {}

// TODO FFI callback type (gc methods)
#end

