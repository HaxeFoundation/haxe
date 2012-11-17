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
#if !(core_api || cross)
#error "Please don't add haxe/std to your classpath, instead set HAXE_LIBRARY_PATH env var"
#end

/**
	The Std class provides standard methods for manipulating basic types.
**/
extern class Std {

	/**
		Tells if a value v is of the type t.
	**/
	public static function is( v : Dynamic, t : Dynamic ) : Bool;

	/**
		Convert any value to a String
	**/
	public static function string( s : Dynamic ) : String;

	/**
		Convert a Float to an Int, rounded down.
	**/
	public static function int( x : Float ) : Int;

	/**
		Convert a String to an Int, parsing different possible representations. Returns [null] if could not be parsed.
	**/
	public static function parseInt( x : String ) : Null<Int>;

	/**
		Convert a String to a Float, parsing different possible reprensations.
	**/
	public static function parseFloat( x : String ) : Float;

	/**
		Return a random integer between 0 included and x excluded.
	**/
	public static function random( x : Int ) : Int;


	#if !haxe3
	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String>;
	#end

}
