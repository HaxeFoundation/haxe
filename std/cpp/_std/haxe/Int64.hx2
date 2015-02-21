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



@:native("cpp::Struct< cpp::Int64 >")
extern class Int64Struct { }

abstract Int64( Int64Struct )
{
   public function toString() : String {
		return untyped __cpp__("String( (cpp::Int64)({0}) )", this);
  }

	public static inline function make( high : Int, low : Int ) : Int64 {
      return untyped __cpp__("cpp::Struct< cpp::Int64 >(( ( (cpp::Int64)((unsigned int){0}) ) << 32 ) | ((unsigned int){1}))",high, low);
	}

	public static inline function ofInt( x : Int ) : Int64 {
		return untyped __cpp__("((cpp::Int64)({0}))", x);
	}

	public static function toInt( x : Int64 ) : Int {
		var high = getHigh(x);
		if( high != 0 ) {
			if( high < 0 )
				return -toInt(neg(x));
			throw "Overflow";
		}
		return getLow(x);
	}

	public static inline function neg( a : Int64 ) : Int64 {
		return untyped __cpp__("(-({0}))",a);
	}


	public static inline function getLow( x : Int64 ) : Int {
		return untyped __cpp__("(int)(({0})&0xffffffff)",x);
	}

	public static function getHigh( x : Int64 ) : Int {
		return  untyped __cpp__("((cpp::Int64)({0}))>>32",x);
	}

	public static inline function add( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) + ({1}))", a, b);
	}

	public static function sub( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) - ({1}))", a, b);
	}

	public static function mul( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) * ({1}))", a, b);
	}

	public static function div( a : Int64, b : Int64 ) : Int64 {
		if (untyped __cpp__("(({0}) == 0)",b) )
			throw "divide by zero";
		return untyped __cpp__("(({0}) / ({1}))", a, b);
	}

	public static function mod( a : Int64, b : Int64 ) : Int64 {
		if (untyped __cpp__("(({0}) == 0)",b) )
			throw "divide by zero";
		return untyped __cpp__("(({0}) % ({1}))", a, b);
	}

	public static inline function shl( a : Int64, b : Int ) : Int64 {
		return untyped __cpp__("(({0}) << ({1}))", a, b);
	}

	public static inline function shr( a : Int64, b : Int ) : Int64 {
		return untyped __cpp__("(({0}) << ({1}))", a, b);
	}

	public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return untyped __cpp__("(((cpp::UInt64)({0})) >> ({1}))", a, b);
	}

	public static inline function and( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) & ({1}))", a, b);
	}

	public static inline function or( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) | ({1}))", a, b);
	}

	public static inline function xor( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}) ^ ({1}))", a, b);
	}


	public static inline function isNeg( a : Int64 ) : Bool {
		return untyped __cpp__("(({0}) < 0)", a);
	}

	public static inline function isZero( a : Int64 ) : Bool {
		return untyped __cpp__("(({0}) == 0)", a);
	}

	public static function compare( a : Int64, b : Int64 ) : Int {
		return untyped __cpp__("( ({0}) < ({1}) ? -1 : ({0})==({1}) ? 0 : 1)", a, b, a, b);
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static function ucompare( a : Int64, b : Int64 ) : Int {
		return untyped __cpp__("( (cpp::UInt64)({0}) < (cpp::UInt64)({1}) ? -1 : ({0})==({1}) ? 0 : 1)", a, b, a, b);
	}

	public static function toStr( a : Int64 ) : String {
		return untyped __cpp__("String( (cpp::Int64)({0}) )", a);
	}

}

