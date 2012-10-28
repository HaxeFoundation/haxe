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
package haxe;

@:native("cpp.CppInt32__") extern class Int32 {
	public static  function make( a : Int, b : Int ) : Int32;
	public static  function ofInt( x : Int ) : Int32;
	public static  function toInt( x : Int32 ) : Int;
	public static  function add( a : Int32, b : Int32 ) : Int32;
	public static  function sub( a : Int32, b : Int32 ) : Int32;
	public static  function mul( a : Int32, b : Int32 ) : Int32;
	public static  function div( a : Int32, b : Int32 ) : Int32;
	public static  function mod( a : Int32, b : Int32 ) : Int32;
	public static  function shl( a : Int32, b : Int ) : Int32;
	public static  function shr( a : Int32, b : Int ) : Int32;
	public static  function ushr( a : Int32, b : Int ) : Int32;
	public static  function and( a : Int32, b : Int32 ) : Int32;
	public static  function or( a : Int32, b : Int32 ) : Int32;
	public static  function xor( a : Int32, b : Int32 ) : Int32;
	public static  function neg( a : Int32 ) : Int32;
	public static  function complement( a : Int32 ) : Int32;
	public static  function compare( a : Int32, b : Int32 ) : Int;
	public static  function isNeg( a : Int32 ) : Bool;
	public static  function isZero( a : Int32 ) : Bool;
	public static  function ucompare( a : Int32, b : Int32 ) : Int;
	public static  function toNativeInt(a:Int32) : Int;
}

