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
package cs;
import cs.StdTypes.Int64;

/**
	This type represents pointer types for C# function parameters. It should only
	be used inside an unsafe context (not checked by the Haxe compiler)

	C# code:
		int[] src;
		fixed (int* pSrc = src)
		{
			...
		}
	Haxe code:
		var src:NativeArray<Int>;
		cs.Lib.fixed({
			var pSrc:cs.Pointer<Int> = cs.Lib.pointerOfArray(src);
			...
		});

**/
#if !unsafe
#error "You need to define 'unsafe' to be able to use unsafe code in hxcs"
#else
@:runtimeValue @:coreType abstract Pointer<T> from Int64 from PointerAccess<T> to PointerAccess<T>
{
	@:op(A+B) public static function addIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A+B) public static function addp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A*B) public static function mulIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A*B) public static function mulp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A%B) public static function modIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A%B) public static function modp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A-B) public static function subIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A-B) public static function subp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A/B) public static function divIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A/B) public static function divp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A|B) public static function orIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A|B) public static function orp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A^B) public static function xorIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A^B) public static function xorp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A&B) public static function andIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A&B) public static function andp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A<<B) public static function shlIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A<<B) public static function shlp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;
	@:op(A>>B) public static function shrIp<T>(lhs:Pointer<T>, rhs:Int):Pointer<T>;
	@:op(A>>B) public static function shrp<T>(lhs:Pointer<T>, rhs:Int64):Pointer<T>;

	@:op(A>B) public static function gtp<T>(lhs:Pointer<T>, rhs:Pointer<T>):Bool;
	@:op(A>=B) public static function gtep<T>(lhs:Pointer<T>, rhs:Pointer<T>):Bool;
	@:op(A<B) public static function ltp<T>(lhs:Pointer<T>, rhs:Pointer<T>):Bool;
	@:op(A<=B) public static function ltep<T>(lhs:Pointer<T>, rhs:Pointer<T>):Bool;

	@:op(~A) public static function bnegp<T>(t:Pointer<T>):Pointer<T>;
	@:op(A++) public static function prepp<T>(t:Pointer<T>):Pointer<T>;
	@:op(A--) public static function prenn<T>(t:Pointer<T>):Pointer<T>;
	@:op(++A) public static function postpp<T>(t:Pointer<T>):Pointer<T>;
	@:op(--A) public static function postnn<T>(t:Pointer<T>):Pointer<T>;

	/**
		Returns a `cs.PointerAccess` type, which in turn allows the underlying Pointer's
		fields to be accessed.
	 **/
	// @:analyzer(no_simplification)
	public var acc(get,never):PointerAccess<T>;

	// @:analyzer(no_simplification)
	@:extern inline private function get_acc():PointerAccess<T> return (cast this : PointerAccess<T>);

	// backwards compatibility
	inline public function add(i:Int):Pointer<T>
	{
		return this + i;
	}

	@:arrayAccess public static function getIp<T>(p:Pointer<T>, at:Int):T;
	@:arrayAccess public static function setIp<T>(p:Pointer<T>, at:Int, val:T):T;
	@:arrayAccess public static function getp<T>(p:Pointer<T>, at:Int64):T;
	@:arrayAccess public static function setp<T>(p:Pointer<T>, at:Int64, val:T):T;
}

@:forward abstract PointerAccess<T>(T)
{
}
#end
