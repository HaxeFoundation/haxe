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
package haxe;

import haxe.Int64Helper;

@:notNull


@:include("cpp/Int64.h")
@:native("cpp::Int64Struct")
private extern class ___Int64 {
   public function get():cpp.Int64;

   @:native("_hx_int64_make")
   public static function make(high:Int32, low:Int32): __Int64 return null;

   @:native(" ::cpp::Int64Struct")
   public static function ofInt(value:Int): __Int64 return null;

   @:native(" ::cpp::Int64Struct::is")
   public static function is(d:Dynamic):Bool return false;

   @:native("_hx_int64_is_neg")
   public static function isNeg(a:__Int64):Bool return false;

   @:native("_hx_int64_is_zero")
   public static function isZero(a:__Int64):Bool return false;

   @:native("_hx_int64_compare")
   public static function compare(a:__Int64, b:__Int64):Int return 0;

   @:native("_hx_int64_ucompare")
   public static function ucompare(a:__Int64, b:__Int64):Int return 0;

   @:native("_hx_int64_to_string")
   public static function toString(a:__Int64):String return null;

   @:native("_hx_int64_neg")
   public static function neg(a:__Int64): __Int64 return null;

   @:native("_hx_int64_pre_increment")
   public static function preIncrement(a:__Int64): __Int64 return null;

   @:native("_hx_int64_post_increment")
   public static function postIncrement(a:__Int64): __Int64 return null;

   @:native("_hx_int64_pre_decrement")
   public static function preDecrement(a:__Int64): __Int64 return null;

   @:native("_hx_int64_post_decrement")
   public static function postDecrement(a:__Int64): __Int64 return null;

   @:native("_hx_int64_add")
   public static function add(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_add")
   public static function addInt(a:__Int64, b:Int): __Int64 return null;

   @:native("_hx_int64_sub")
   public static function sub(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_sub")
   public static function subInt(a:__Int64, b:Int): __Int64 return null;

   @:native("_hx_int64_sub")
   public static function intSub(a:Int, b:__Int64): __Int64 return null;

   @:native("_hx_int64_mul")
   public static function mul(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_div")
   public static function div(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_mod")
   public static function mod(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_eq")
   public static function eq(a:__Int64, b:__Int64): Bool return null;

   @:native("_hx_int64_eq")
   public static function eqInt(a:__Int64, b:Int): Bool return null;

   @:native("_hx_int64_neq")
   public static function neq(a:__Int64, b:__Int64): Bool return null;

   @:native("_hx_int64_neq")
   public static function neqInt(a:__Int64, b:Int): Bool return null;

   @:native("_hx_int64_complement")
   public static function complement(a:__Int64): __Int64 return null;

   @:native("_hx_int64_and")
   public static function bitAnd(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_or")
   public static function bitOr(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_xor")
   public static function bitXor(a:__Int64, b:__Int64): __Int64 return null;

   @:native("_hx_int64_shl")
   public static function shl(a:__Int64, b:Int): __Int64 return null;

   @:native("_hx_int64_shr")
   public static function shr(a:__Int64, b:Int): __Int64 return null;

   @:native("_hx_int64_ushr")
   public static function ushr(a:__Int64, b:Int): __Int64 return null;

   @:native("_hx_int64_high")
   public static function high(a:__Int64):Int32 return 0;

   @:native("_hx_int64_low")
   public static function low(a:__Int64):Int32 return 0;

}

private typedef __Int64 = ___Int64;

@:coreApi
abstract Int64( __Int64 ) from __Int64 to __Int64
{
   /**
		Makes a copy of `this` Int64.
	**/
	public #if !cppia inline #end function copy():Int64 return this;


	public static #if !cppia inline #end function make( high : Int32, low : Int32 ) : Int64 {
      return __Int64.make(high,low);
	}

	@:from public static #if !cppia inline #end function ofInt( x : Int ) : Int64 {
      return __Int64.ofInt(x);
	}


	/**
		Returns an Int with the value of the Int64 `x`.
		Throws an exception  if `x` cannot be represented in 32 bits.
	**/
	public static #if !cppia inline #end function toInt( x : Int64 ) : Int {
		if( x.high != x.low >> 31 )
			throw "Overflow";

		return x.low;
	}

	/**
		Returns whether the value `val` is of type `haxe.Int64`
	**/
	public static #if !cppia inline #end function is( val : Dynamic ) : Bool
      return __Int64.is(val);

	/**
		Returns the high 32-bit word of `x`.
	**/
	@:deprecated("Use high instead")
	public static #if !cppia inline #end function getHigh( x : Int64 ) : Int32
		return x.high;

	/**
		Returns the low 32-bit word of `x`.
	**/
	@:deprecated("Use low instead")
	public static #if !cppia inline #end function getLow( x : Int64 ) : Int32
		return x.low;

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static #if !cppia inline #end function isNeg( x : Int64) : Bool
      return __Int64.isNeg(x);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static #if !cppia inline #end function isZero( x : Int64 ) : Bool
      return __Int64.isZero(x);

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static #if !cppia inline #end function compare( a : Int64, b : Int64 ) : Int
      return __Int64.compare(a,b);

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static #if !cppia inline #end function ucompare( a : Int64, b : Int64 ) : Int
      return __Int64.ucompare(a,b);

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	public static #if !cppia inline #end function toStr(x:Int64) : String
		return x.toString();

	private #if !cppia inline #end function toString() : String
      return __Int64.toString(this);

	public static function parseString( sParam : String ) : Int64 {
		return Int64Helper.parseString( sParam );
	}

	public static function fromFloat( f : Float ) : Int64 {
		return Int64Helper.fromFloat( f );
	}

	/**
		Performs signed integer divison of `dividend` by `divisor`.
		Returns `{ quotient : Int64, modulus : Int64 }`.
	**/
	public static function divMod( dividend : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
	{
      var q = dividend/divisor;

      if (isZero(divisor))
	       throw "divide by zero";

      var m = dividend - q*divisor;

      return { quotient : q, modulus : m };
	}

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) public static #if !cppia inline #end function neg( x : Int64 ) : Int64
      return __Int64.neg(x);

	@:op(++A) private inline  function preIncrement() : Int64 {
      #if cppia
      this = this + make(0,1);
      return this;
      #else
		return __Int64.preIncrement(this);
      #end
	}

	@:op(A++) private inline function postIncrement() : Int64 {
      #if cppia
      var result = this;
      this = this + make(0,1);
      return result;
      #else
		return __Int64.postIncrement(this);
      #end
	}

	@:op(--A) private inline function preDecrement() : Int64 {
      #if cppia
      untyped this = this - make(0,1);
      return this;
      #else
		return __Int64.preDecrement(this);
      #end
	}

	@:op(A--) private inline function postDecrement() : Int64 {
      #if cppia
      var result = this;
      this = this - make(0,1);
      return result;
      #else
		return __Int64.postDecrement(this);
      #end
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static #if !cppia inline #end function add( a : Int64, b : Int64 ) : Int64
      return __Int64.add(a,b);

	@:op(A + B) @:commutative private static #if !cppia inline #end function addInt( a : Int64, b : Int ) : Int64
      return __Int64.addInt(a,b);

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static #if !cppia inline #end function sub( a : Int64, b : Int64 ) : Int64 {
      return __Int64.sub(a,b);
	}

	@:op(A - B) private static #if !cppia inline #end function subInt( a : Int64, b : Int ) : Int64
		return __Int64.subInt( a, b );

	@:op(A - B) private static #if !cppia inline #end function intSub( a : Int, b : Int64 ) : Int64
		return __Int64.intSub( a, b );

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static #if !cppia inline #end function mul( a : Int64, b : Int64 ) : Int64
		return __Int64.mul( a, b );

	@:op(A * B) @:commutative private static #if !cppia inline #end function mulInt( a : Int64, b : Int ) : Int64
		return mul( a, b );

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static #if !cppia inline #end function div( a : Int64, b : Int64 ) : Int64 {
		if (__Int64.isZero(b))
			throw "divide by zero";
		return __Int64.div(a,b);
	}


	@:op(A / B) private static #if !cppia inline #end function divInt( a : Int64, b : Int ) : Int64
		return div( a, b );

	@:op(A / B) private static #if !cppia inline #end function intDiv( a : Int, b : Int64 ) : Int64
		return toInt(div( a, b ));

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static #if !cppia inline #end function mod( a : Int64, b : Int64 ) : Int64
   {
		if (__Int64.isZero(b))
			throw "divide by zero";
		return __Int64.mod(a, b);
   }

	@:op(A % B) private static #if !cppia inline #end function modInt( a : Int64, b : Int ) : Int64
		return toInt(mod( a, b ));

	@:op(A % B) private static #if !cppia inline #end function intMod( a : Int, b : Int64 ) : Int64
		return toInt(mod( a, b ));

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static #if !cppia inline #end function eq( a : Int64, b : Int64 ) : Bool
      return __Int64.eq(a,b);

	@:op(A == B) @:commutative private static #if !cppia inline #end function eqInt( a : Int64, b : Int ) : Bool
		return __Int64.eqInt( a, b );

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static #if !cppia inline #end function neq( a : Int64, b : Int64 ) : Bool
		return __Int64.neq( a, b );

	@:op(A != B) @:commutative private static #if !cppia inline #end function neqInt( a : Int64, b : Int ) : Bool
		return neq( a, b );

	@:op(A < B) private static #if !cppia inline #end function lt( a : Int64, b : Int64 ) : Bool
		return compare(a, b) < 0;

	@:op(A < B) private static #if !cppia inline #end function ltInt( a : Int64, b : Int ) : Bool
		return lt(a, b);

	@:op(A < B) private static #if !cppia inline #end function intLt( a : Int, b : Int64 ) : Bool
		return lt(a, b);

	@:op(A <= B) private static #if !cppia inline #end function lte( a : Int64, b : Int64 ) : Bool
		return compare(a, b) <= 0;

	@:op(A <= B) private static #if !cppia inline #end function lteInt( a : Int64, b : Int ) : Bool
		return lte(a, b);

	@:op(A <= B) private static #if !cppia inline #end function intLte( a : Int, b : Int64 ) : Bool
		return lte(a, b);

	@:op(A > B) private static #if !cppia inline #end function gt( a : Int64, b : Int64 ) : Bool
		return compare(a, b) > 0;

	@:op(A > B) private static #if !cppia inline #end function gtInt( a : Int64, b : Int ) : Bool
		return gt(a, b);

	@:op(A > B) private static #if !cppia inline #end function intGt( a : Int, b : Int64 ) : Bool
		return gt( a, b );

	@:op(A >= B) private static #if !cppia inline #end function gte( a : Int64, b : Int64 ) : Bool
		return compare(a, b) >= 0;

	@:op(A >= B) private static #if !cppia inline #end function gteInt( a : Int64, b : Int ) : Bool
		return gte(a, b);

	@:op(A >= B) private static #if !cppia inline #end function intGte( a : Int, b : Int64 ) : Bool
		return gte(a, b);

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static #if !cppia inline #end function complement( a : Int64 ) : Int64
      return __Int64.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static #if !cppia inline #end function and( a : Int64, b : Int64 ) : Int64
      return __Int64.bitAnd(a,b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static #if !cppia inline #end function or( a : Int64, b : Int64 ) : Int64
      return __Int64.bitOr(a,b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static #if !cppia inline #end function xor( a : Int64, b : Int64 ) : Int64
      return __Int64.bitXor(a,b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static #if !cppia inline #end function shl( a : Int64, b : Int ) : Int64
      return __Int64.shl(a,b);

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static #if !cppia inline #end function shr( a : Int64, b : Int) : Int64
      return __Int64.shr(a,b);

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static #if !cppia inline #end function ushr( a : Int64, b : Int ) : Int64
      return __Int64.ushr(a,b);

	public var high(get, never) : Int32;
	private #if !cppia inline #end function get_high() : Int32
      return __Int64.high(this);

	public var low(get, never) : Int32;
	private #if !cppia inline #end function get_low() : Int32
      return __Int64.low(this);

}

