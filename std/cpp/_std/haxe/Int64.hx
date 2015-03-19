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



@:notNull

@:native("cpp::Int64Struct")
private extern class ___Int64 { public function get():cpp.Int64; }

private typedef __Int64 = ___Int64;

@:coreApi
abstract Int64( __Int64 ) from __Int64 to __Int64
{
   /**
		Makes a copy of `this` Int64.
	**/
	public #if !cppua inline #end function copy():Int64 return this;


	public static #if !cppia inline #end function make( high : Int32, low : Int32 ) : Int64 {
      return untyped __cpp__("cpp::Int64Struct(( ( (cpp::Int64)((unsigned int){0}) ) << 32 ) | ((unsigned int){1}))",high, low);
	}

	@:from public static function ofInt( x : Int ) : Int64 {
		return untyped __cpp__("((cpp::Int64)({0}))", x);
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
      return untyped __cpp__("(cpp::Int64Struct::is({0}))",val);

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
		return untyped __cpp__("(({0}.get()) < 0)", x);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static #if !cppia inline #end function isZero( x : Int64 ) : Bool
		return untyped __cpp__("(({0}.get()) == 0)", x);

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static #if !cppia inline #end function compare( a : Int64, b : Int64 ) : Int {
		return untyped __cpp__("( ({0}.get()) < ({1}.get()) ? -1 : ({0})==({1}) ? 0 : 1)", a, b, a, b);
	}

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static #if !cppia inline #end function ucompare( a : Int64, b : Int64 ) : Int {
		return untyped __cpp__("( (cpp::UInt64)({0}.get()) < (cpp::UInt64)({1}.get()) ? -1 : ({0}.get())==({1}.get()) ? 0 : 1)", a, b, a, b);
	}

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	public static #if !cppia inline #end function toStr(x:Int64) : String
		return x.toString();

	private function toString() : String
	{
		return untyped __cpp__("String( ({0}).get() )", this);
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
	@:op(-A) public static #if !cppia inline #end function neg( x : Int64 ) : Int64 {
		return untyped __cpp__("(-({0}.get()))",x);
	}

	@:op(++A) private inline  function preIncrement() : Int64 {
      #if cppia
      this = this + make(0,1);
      return this;
      #else
		return untyped __cpp__("(++({0}.get()))",this);
      #end
	}

	@:op(A++) private inline function postIncrement() : Int64 {
      #if cppia
      var result = this;
      this = this + make(0,1);
      return result;
      #else
		return untyped __cpp__("(({0}.get())++)",this);
      #end
	}

	@:op(--A) private inline function preDecrement() : Int64 {
      #if cppia
      untyped this = this - make(0,1);
      return this;
      #else
		return untyped __cpp__("(--({0}.get()))",this);
      #end
	}

	@:op(A--) private inline function postDecrement() : Int64 {
      #if cppia
      var result = this;
      this = this - make(0,1);
      return result;
      #else
		return untyped __cpp__("(({0}.get())--)",this);
      #end
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static #if !cppia inline #end function add( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}.get()) + ({1}.get()))", a, b);
	}

	@:op(A + B) @:commutative private static #if !cppia inline #end function addInt( a : Int64, b : Int ) : Int64
		return add( a, b );

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static #if !cppia inline #end function sub( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}.get()) - ({1}.get()))", a, b);
	}

	@:op(A - B) private static #if !cppia inline #end function subInt( a : Int64, b : Int ) : Int64
		return sub( a, b );

	@:op(A - B) private static #if !cppia inline #end function intSub( a : Int, b : Int64 ) : Int64
		return sub( a, b );

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static #if !cppia inline #end function mul( a : Int64, b : Int64 ) : Int64 {
		return untyped __cpp__("(({0}.get()) * ({1}.get()))", a, b);
	}

	@:op(A * B) @:commutative private static #if !cppia inline #end function mulInt( a : Int64, b : Int ) : Int64
		return mul( a, b );

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static function div( a : Int64, b : Int64 ) : Int64 {
		if (untyped __cpp__("(({0}.get()) == 0)",b) )
			throw "divide by zero";
		return untyped __cpp__("(({0}.get()) / ({1}.get()))", a, b);
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
		if (untyped __cpp__("(({0}.get()) == 0)",b) )
			throw "divide by zero";
		return untyped __cpp__("(({0}.get()) % ({1}.get()))", a, b);
   }

	@:op(A % B) private static #if !cppia inline #end function modInt( a : Int64, b : Int ) : Int64
		return toInt(mod( a, b ));

	@:op(A % B) private static #if !cppia inline #end function intMod( a : Int, b : Int64 ) : Int64
		return toInt(mod( a, b ));

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static #if !cppia inline #end function eq( a : Int64, b : Int64 ) : Bool
		return untyped __cpp__("(({0}.get()) == ({1}.get()))", a, b);

	@:op(A == B) @:commutative private static #if !cppia inline #end function eqInt( a : Int64, b : Int ) : Bool
		return eq( a, b );

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static #if !cppia inline #end function neq( a : Int64, b : Int64 ) : Bool
		return untyped __cpp__("(({0}.get()) != ({1}.get()))", a, b);

	@:op(A != B) @:commutative private static #if !cppia inline #end function neqInt( a : Int64, b : Int ) : Bool
		return neq(a, b);

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
		return untyped __cpp__("(~({0}.get()))", a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static #if !cppia inline #end function and( a : Int64, b : Int64 ) : Int64
		return untyped __cpp__("(({0}) & ({1}))", a, b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static #if !cppia inline #end function or( a : Int64, b : Int64 ) : Int64
		return untyped __cpp__("(({0}) | ({1}))", a, b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static #if !cppia inline #end function xor( a : Int64, b : Int64 ) : Int64
		return untyped __cpp__("(({0}) ^ ({1}))", a, b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static #if !cppia inline #end function shl( a : Int64, b : Int ) : Int64
		return untyped __cpp__("(({0}) << ({1}))", a, (b&63));

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static #if !cppia inline #end function shr( a : Int64, b : Int) : Int64
		return untyped __cpp__("(({0}) >> ({1}))", a, (b&63));

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static #if !cppia inline #end function ushr( a : Int64, b : Int ) : Int64
		return untyped __cpp__("(((cpp::UInt64)({0})) >> ({1}))", a, (b&63));

	public var high(get, never) : Int32;
	private #if !cppia inline #end function get_high() : Int32
		return  untyped __cpp__("(int)(((cpp::Int64)({0}))>>32)",this);

	public var low(get, never) : Int32;
	private #if !cppia inline #end function get_low() : Int32
		return untyped __cpp__("(int)(({0})&0xffffffff)", this);

}

