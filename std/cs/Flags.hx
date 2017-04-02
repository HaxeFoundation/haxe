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

/**
	Use this type to have access to the bitwise operators of C# enums that have a `cs.system.FlagsAttribute` attribute.

	Usage example:

	```haxe
		import cs.system.reflection.BindingFlags;
		var binding = new Flags(BindingFlags.Public) | BindingFlags.Static | BindingFlags.NonPublic;
	```
 **/
abstract Flags<T : EnumValue>(T) from T to T
{

	/**
		Creates a new `Flags` type with an optional initial value. If no initial value was specified,
		the default enum value for an empty flags attribute is specified
	 **/
	@:extern inline public function new(?initial:T)
		this = initial;

	/**
		Accessible through the bitwise OR operator (`|`). Returns a new `Flags` type with the flags
		passed at `flags` added to it.
	 **/
	@:op(A|B) @:extern inline public function add(flags:Flags<T>):Flags<T>
	{
		return new Flags(underlying() | flags.underlying());
	}

	/**
		Accessible through the bitwise AND operator (`&`). Returns a new `Flags` type with
		the flags that are set on both `this` and `flags`
	 **/
	@:op(A&B) @:extern inline public function bitAnd(flags:Flags<T>):Flags<T>
	{
		return new Flags(underlying() & flags.underlying());
	}

	/**
		Accessible through the bitwise XOR operator (`^`).
	 **/
	@:op(A^B) @:extern inline public function bitXor(flags:Flags<T>):Flags<T>
	{
		return new Flags(underlying() & flags.underlying());
	}

	/**
		Accesible through the bitwise negation operator (`~`). Returns a new `Flags` type
		with all unset flags as set - but the ones that are set already.
	 **/
	@:op(~A) @:extern inline public function bitNeg():Flags<T>
	{
		return new Flags(~underlying());
	}

	/**
		Returns a new `Flags` type with all flags set by `flags` unset
	 **/
	@:extern inline public function remove(flags:Flags<T>):Flags<T>
	{
		return new Flags(underlying() & ~flags.underlying());
	}

	/**
		Returns whether `flag` is present on `this` type
	 **/
	@:extern inline public function has(flag:T):Bool
	{
		return underlying() & new Flags(flag).underlying() != null;
	}

	/**
		Returns whether `this` type has any flag set by `flags` also set
	 **/
	@:extern inline public function hasAny(flags:Flags<T>):Bool
	{
		return underlying() & flags.underlying() != null;
	}

	/**
		Returns whether `this` type has all flags set by `flags` also set
	 **/
	@:extern inline public function hasAll(flags:Flags<T>):Bool
	{
		return underlying() & flags.underlying() == flags.underlying();
	}

	@:extern inline private function underlying():EnumUnderlying<T>
		return this;
}

@:coreType private abstract EnumUnderlying<T> from T to T
{
	@:op(A|B) public static function or<T>(lhs:EnumUnderlying<T>, rhs:EnumUnderlying<T>):T;
	@:op(A^B) public static function xor<T>(lhs:EnumUnderlying<T>, rhs:EnumUnderlying<T>):T;
	@:op(A&B) public static function and<T>(lhs:EnumUnderlying<T>, rhs:EnumUnderlying<T>):T;
	@:op(~A) public static function bneg<T>(t:EnumUnderlying<T>):T;
}
