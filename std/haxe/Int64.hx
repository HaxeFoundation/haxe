/*
 * Copyright (C)2005-2013 Haxe Foundation
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

abstract Int64(Int64Data) from Int64Data to Int64Data
{
	inline function new(x:Int64Data) this = x;

	inline static function RAW(x:Int64Data):Int64 return new Int64(x);

	public static var MAX_VALUE:Int64 = new Int64(Int64Data.make(0x7fffffff, -1));
	public static var MIN_VALUE:Int64 = new Int64(Int64Data.make(0x80000000, 0));

	static inline var TWO_32:Float =  4294967296.0;

	public static inline function make(a:Int, b:Int) {
		return RAW(Int64Data.make(a, b));
	}

	@:from public static inline function fromInt(x:Int):Int64 {
		return RAW(Int64Data.ofInt(x));
	}

	@:from public static inline function fromFloat(x:Float):Int64 {

		if (x > MAX_VALUE.toFloat()) throw "overflow";

		if (x < 0)
			return neg(fromFloat( -x));
		else
		{
			var high = Std.int(x / TWO_32);
			var low = Std.int(x - high * TWO_32);
			return RAW(Int64Data.make(high, low));
		}
	}

	public static function parse(str:String):Int64 {
		var out          = fromInt( Std.parseInt( str ) );
		var c            = toString( out );
		if( c != str ) out = readDigits( str );
		return out;
	}

	// taken and modified from Unserializer
	inline static function readDigits( buf: String ):Int64
	{
		var k: Int64    = 0;
		var s           = false;
		var pos         = 0;
		var fpos        = pos;

		while( true )
		{
			var c = buf.charCodeAt( pos );
			if( c == null )
				break;
			if( c == "-".code ) { // negative sign
				if( pos != fpos )
					break;
				s = true;
				pos++;
				continue;
			}
			c -= "0".code;
			if( c < 0 || c > 9 )
				break;
			k = ( k * 10 ) + c;
			pos++;
		}
		if( s )
			k = neg( k );
		return k;
	}

	/*
	 * warning: only valid if in the range [-2^53, 2^53]
	 */
	public inline function toFloat():Float {
		var high:Float = Int64Data.getHigh(this)* TWO_32;
		var low:Float = Int64Data.getLow(this);
		return high + low;
	}

	public inline function toInt():Int {
		return Int64Data.toInt(this);
	}

	@:op(A+B) public static inline function add(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.add(a, b));
	}

	@:commutative @:op(A+B) public static inline function addf(a:Int64, b:Float):Int64 {
		return add(a, fromFloat(b));
	}

	@:commutative @:op(A+B) public static inline function addi(a:Int64, b:Int):Int64 {
		return add(a, fromInt(b));
	}

	@:op(A-B) public static inline function sub(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.sub(a, b));
	}

	@:op(A-B) public static inline function subf(a:Int64, b:Float):Int64 {
		return sub(a, fromFloat(b));
	}

	@:op(A-B) public static inline function subi(a:Int64, b:Int):Int64 {
		return sub(a, fromInt(b));
	}

	@:op(A-B) public static inline function fsub(a:Float, b:Int64):Int64 {
		return sub(fromFloat(a), b);
	}

	@:op(A-B) public static inline function isub(a:Int, b:Int64):Int64 {
		return sub(fromInt(a), b);
	}

	@:op(-A) public static inline function neg(x:Int64):Int64 {
		return RAW(Int64Data.neg(x));
	}

	@:op(A*B) public static inline function mul(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.mul(a, b));
	}

	@:commutative @:op(A*B) public static inline function mulf(a:Int64, b:Float):Int64 {
		return mul(a, fromFloat(b));
	}

	@:commutative @:op(A*B) public static inline function muli(a:Int64, b:Int):Int64 {
		return mul(a, fromInt(b));
	}

	@:op(A/B) public static inline function div(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.div(a, b));
	}

	@:op(A/B) public static inline function divf(a:Int64, b:Float):Int64 {
		return div(a, fromFloat(b));
	}

	@:op(A/B) public static inline function divi(a:Int64, b:Int):Int64 {
		return div(a, fromInt(b));
	}

	@:op(A/B) public static inline function fdiv(a:Float, b:Int64):Int64 {
		return div(fromFloat(a), b);
	}

	@:op(A/B) public static inline function idiv(a:Int, b:Int64):Int64 {
		return div(fromInt(a), b);
	}

	@:op(A % B) public static inline function mod(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.mod(a, b));
	}

	@:op(A%B) public static inline function modf(a:Int64, b:Float):Int64 {
		return mod(a, fromFloat(b));
	}

	@:op(A%B) public static inline function modi(a:Int64, b:Int):Int64 {
		return mod(a, fromInt(b));
	}

	@:op(A%B) public static inline function fmod(a:Float, b:Int64):Int64 {
		return mod(fromFloat(a), b);
	}

	@:op(A%B) public static inline function imod(a:Int, b:Int64):Int64 {
		return mod(fromInt(a), b);
	}

	@:op(A==B) public static inline function eq(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) == 0;
	}

	@:commutative @:op(A==B) public static inline function eqf(a:Int64, b:Float):Bool {
		return eq(a, fromFloat(b));
	}

	@:commutative @:op(A == B) public static inline function eqi(a:Int64, b:Int):Bool {
		// check for special case when b == 0
		return b == 0 ? Int64Data.isZero(a) : eq(a, fromInt(b));
	}

	@:op(A!=B) public static inline function neq(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) != 0;
	}

	@:commutative @:op(A!=B) public static inline function neqf(a:Int64, b:Float):Bool {
		return neq(a, fromFloat(b));
	}

	@:commutative @:op(A!=B) public static inline function neqi(a:Int64, b:Int):Bool {
		// check for special case when b == 0
		return b == 0 ? !Int64Data.isZero(a) : neq(a, fromInt(b));
	}

	@:op(A>B) public static inline function gt(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) > 0;
	}

	@:op(A>B) public static inline function gtf(a:Int64, b:Float):Bool {
		return gt(a, fromFloat(b));
	}

	@:op(A>B) public static inline function gti(a:Int64, b:Int):Bool {
		return gt(a, fromInt(b));
	}

	@:op(A>B) public static inline function fgt(a:Float, b:Int64):Bool {
		return gt(fromFloat(a), b);
	}

	@:op(A>B) public static inline function igt(a:Int, b:Int64):Bool {
		return gt(fromInt(a), b);
	}

	@:op(A<B) public static inline function lt(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) < 0;
	}

	@:op(A<B) public static inline function ltf(a:Int64, b:Float):Bool {
		return lt(a, fromFloat(b));
	}

	@:op(A<B) public static inline function lti(a:Int64, b:Int):Bool {
		return b == 0 ? Int64Data.isNeg(a) : lt(a, fromInt(b));
	}

	@:op(A<B) public static inline function flt(a:Float, b:Int64):Bool {
		return lt(fromFloat(a), b);
	}

	@:op(A<B) public static inline function ilt(a:Int, b:Int64):Bool {
		return lt(fromInt(a), b);
	}

	@:op(A>=B) public static inline function gte(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) >= 0;
	}

	@:op(A>=B) public static inline function gtef(a:Int64, b:Float):Bool {
		return gte(a, fromFloat(b));
	}

	@:op(A>=B) public static inline function gtei(a:Int64, b:Int):Bool {
		return b == 0 ? !Int64Data.isNeg(a) : gte(a, fromInt(b));
	}

	@:op(A>=B) public static inline function fgte(a:Float, b:Int64):Bool {
		return gte(fromFloat(a), b);
	}

	@:op(A>=B) public static inline function igte(a:Int, b:Int64):Bool {
		return gte(fromInt(a), b);
	}

	@:op(A<=B) public static inline function lte(a:Int64, b:Int64):Bool {
		return Int64Data.compare(a, b) <= 0;
	}

	@:op(A<=B) public static inline function ltef(a:Int64, b:Float):Bool {
		return lte(a, fromFloat(b));
	}

	@:op(A<=B) public static inline function ltei(a:Int64, b:Int):Bool {
		return lte(a, fromInt(b));
	}

	@:op(A<=B) public static inline function flte(a:Float, b:Int64):Bool {
		return lte(fromFloat(a), b);
	}

	@:op(A<=B) public static inline function ilte(a:Int, b:Int64):Bool {
		return lte(fromInt(a), b);
	}

	@:op(A<<B) public static inline function shl(a:Int64, b:Int):Int64 {
		return RAW(Int64Data.shl(a, b));
	}

	@:op(A>>B) public static inline function shr(a:Int64, b:Int):Int64 {
		return RAW(Int64Data.shr(a, b));
	}

	@:op(A>>>B) public static inline function ushr(a:Int64, b:Int):Int64 {
		return RAW(Int64Data.ushr(a, b));
	}

	@:op(A&B) public static inline function and(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.and(a, b));
	}

	@:commutative @:op(A&B) public static inline function andi(a:Int64, b:Int):Int64 {
		return and(a, fromInt(b));
	}

	@:op(A|B) public static inline function or(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.or(a, b));
	}

	@:commutative @:op(A|B) public static inline function ori(a:Int64, b:Int):Int64 {
		return or(a, fromInt(b));
	}

	@:op(A^B) public static inline function xor(a:Int64, b:Int64):Int64 {
		return RAW(Int64Data.xor(a, b));
	}

	@:commutative @:op(A^B) public static inline function xori(a:Int64, b:Int):Int64 {
		return xor(a, fromInt(b));
	}

	public inline function toString():String {
		return Int64Data.toStr(this);
	}
}
