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
// standard Haxe types

/**
	The standard Void type. Only `null` values can be of the type `Void`.
**/
@:coreType abstract Void { }

/**
	The standard Float type, this is a double-precision IEEE 64bit float.
	
	On static targets, null cannot be assigned to Float. If this is necessary,
	`Null<Float>` can be used instead.	
**/
@:coreType @:notNull @:runtimeValue abstract Float { }

/**
	The standard Int type. Its precision depends on the platform.
	
	On static targets, null cannot be assigned to Int. If this is necessary,
	`Null<Int>` can be used instead.
**/
@:coreType @:notNull @:runtimeValue abstract Int to Float { }

#if (flash9 || flash9doc || cs)
/**
	The unsigned Int type is only defined for Flash9 and C#. It's currently
	handled the same as a normal Int.
**/
@:coreType @:notNull @:runtimeValue abstract UInt to Int from Int { }
#else
/**
	The unsigned Int type is only defined for Flash9 and C#. 
	Simulate it for other platforms.
**/
abstract UInt(Int) from Int {

	@:commutative @:op(A + B) public static inline function addWithFloat(a:UInt, b:Float):Float {
		return a.toFloat() + b;
	}

	@:commutative @:op(A * B) public static inline function mulWithFloat(a:UInt, b:Float):Float {
		return a.toFloat() * b;
	}

	@:op(A / B) public static inline function divFloat(a:UInt, b:Float):Float {
		return a.toFloat() / b;
	}

	@:op(A / B) public static inline function floatDiv(a:Float, b:UInt):Float {
		return a / b.toFloat();
	}

	@:op(A - B) public static inline function subFloat(a:UInt, b:Float):Float {
		return a.toFloat() - b;
	}

	@:op(A - B) public static inline function floatSub(a:Float, b:UInt):Float {
		return a - b.toFloat();
	}

	@:op(A > B) public static inline function gtFloat(a:UInt, b:Float):Bool {
		return a.toFloat() > b;
	}

	@:op(A >= B) public static inline function gteFloat(a:UInt, b:Float):Bool {
		return a.toFloat() >= b;
	}


	@:op(A > B) public static inline function floatGt(a:Float, b:UInt):Bool {
		return a > b.toFloat();
	}

	@:op(A >= B) public static inline function floatGte(a:Float, b:UInt):Bool {
		return a >= b.toFloat();
	}

	@:op(A < B) public static inline function ltFloat(a:UInt, b:Float):Bool {
		return a.toFloat() < b;
	}

	@:op(A <= B) public static inline function lteFloat(a:UInt, b:Float):Bool {
		return a.toFloat() <= b;
	}


	@:op(A < B) public static inline function floatLt(a:Float, b:UInt):Bool {
		return a < b.toFloat();
	}

	@:op(A <= B) public static inline function floatLte(a:Float, b:UInt):Bool {
		return a <= b.toFloat();
	}

	@:op(A % B) public static inline function modFloat(a:UInt, b:Float):Float {
		return a.toFloat() % b;
	}

	@:op(A % B) public static inline function floatMod(a:Float, b:UInt):Float {
		return a % b.toFloat();
	}

	@:op(A + B) public static inline function add(a:UInt, b:UInt):UInt {
		return a.toInt() + b.toInt();
	}

	@:op(A / B) public static inline function div(a:UInt, b:UInt):Float {
		return a.toInt() / b.toInt();
	}

	@:op(A * B) public static inline function mul(a:UInt, b:UInt):UInt {
		return a.toInt() * b.toInt();
	}

	@:op(A - B) public static inline function sub(a:UInt, b:UInt):UInt {
		return a.toInt() - b.toInt();
	}

	@:op(A > B) public static inline function gt(a:UInt, b:UInt):Bool {
		if (a.toInt() < 0) {
			if (b.toInt() >= 0) {
				return true;
			}
			else {
				return a.toInt() > b.toInt();
			}
		}
		else {
			if (b.toInt() >= 0) {
				return a.toInt() > b.toInt();
			}
			else {
				return false;
			}
		}
	}

	@:op(A >= B) public static inline function gte(a:UInt, b:UInt):Bool {
		if (a.toInt() < 0) {
			if (b.toInt() >= 0) {
				return true;
			}
			else {
				return a.toInt() >= b.toInt();
			}
		}
		else {
			if (b.toInt() >= 0) {
				return a.toInt() >= b.toInt();
			}
			else {
				return false;
			}
		}
	}

	@:op(A < B) public static inline function lt(a:UInt, b:UInt):Bool {
		return gt(b, a);
	}

	@:op(A <= B) public static inline function lte(a:UInt, b:UInt):Bool {
		return gte(b, a);
	}

	@:op(A & B) public static inline function and(a:UInt, b:UInt):UInt {
		return a.toInt() & b.toInt();
	}

	@:op(A | B) public static inline function or(a:UInt, b:UInt):UInt {
		return a.toInt() | b.toInt();
	}

	@:op(A ^ B) public static inline function xor(a:UInt, b:UInt):UInt {
		return a.toInt() ^ b.toInt();
	}

	@:op(A << B) public static inline function shl(a:UInt, b:Int):UInt {
		return a.toInt() << b;
	}

	@:op(A >> B) public static inline function shr(a:UInt, b:UInt):UInt {
		return a.toInt() >> b;
	}

	@:op(A >>> B) public static inline function ushr(a:UInt, b:UInt):UInt {
		return a.toInt() >>> b;
	}

	@:op(A % B) public static inline function mod(a:UInt, b:UInt):UInt {
		return a.toInt() % b.toInt();
	}

	@:op(~A) public inline function negBits():UInt {
		return ~this;
	}

	@:op(++A) public inline function prefixIncrement():UInt {
		return ++this;
	}

	@:op(A++) public inline function postfixIncrement():UInt {
		return this++;
	}

	@:op(--A) public inline function prefixDecrement():UInt {
		return --this;
	}

	@:op(A--) public inline function postfixDecrement():UInt {
		return this--;
	}

	public inline function toString():String {
		return Std.string(toFloat());
	}

	@:to public inline function toDynamic():Dynamic {
		return toFloat();
	}

	@:to public inline function toFloat():Float {
		var int:Int = cast this;
		if (int < 0) {
			return 4294967296.0 + int;
		}
		else {
			return int;
		}
	}

	@:to public inline function toInt():Int {
		return this;
	}

}
#end

#if (java || cs)
@:coreType @:notNull @:runtimeValue abstract Single to Float from Float {}
#end

/**
	`Null` can be useful in two cases. In order to document some methods
	that accepts or can return a `null` value, or for the Flash9 compiler and AS3
	generator to distinguish between base values that can be null and others that
	can't.
**/
typedef Null<T> = T

/**
	The standard Boolean type, which can either be true or false.
	
	On static targets, null cannot be assigned to Bool. If this is necessary,
	`Null<Bool>` can be used instead.
**/
@:coreType @:notNull @:runtimeValue abstract Bool {
}

/**
	Dynamic is a special type which is compatible with all other types.
	
	Use of Dynamic should be minimized as it prevents several compiler
	checks and optimizations.
**/
@:coreType @:runtimeValue abstract Dynamic<T> {
}

/**
	An Iterator is a structure that permits iteration over elements of type T.

	Any class with matching hasNext and next fields is considered an Iterator
	and can then be used e.g. in for-loops. This makes it easy to implement
	custom iterators.
**/
typedef Iterator<T> = {
	
	/**
		Returns false if the iteration is complete, true otherwise.
		
		Usually iteration is considered to be complete if all elements of the
		underlying data structure were handled through calls to next(). However,
		in custom iterators any logic may be used to determine the completion
		state.
	**/
	function hasNext() : Bool;
	
	/**
		Returns the current item of the Iterator and advances to the next one.
		
		This method is not required to check hasNext() first. A call to this
		method while hasNext() is false yields unspecified behavior.
	**/
	function next() : T;
	
}

/**
	An Iterable is a data structure which has an iterator() method.
	See `Lambda` for generic functions on iterable structures.
**/
typedef Iterable<T> = {
	function iterator() : Iterator<T>;
}

/**
	ArrayAccess is used to indicate a class that can be accessed using brackets.
	The type parameter represent the type of the elements stored.
**/
extern interface ArrayAccess<T> { }
