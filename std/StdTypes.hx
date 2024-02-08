/*
 * Copyright (C)2005-2019 Haxe Foundation
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
	The standard `Void` type. Only `null` values can be of the type `Void`.

	@see https://haxe.org/manual/types-void.html
**/
#if jvm
@:runtimeValue
#end
@:coreType abstract Void {}

/**
	The standard `Float` type, this is a double-precision IEEE 64bit float.

	On static targets, `null` cannot be assigned to Float. If this is necessary,
	`Null<Float>` can be used instead.

	`Std.int` converts a `Float` to an `Int`, rounded towards 0.
	`Std.parseFloat` converts a `String` to a `Float`.

	@see https://haxe.org/manual/types-basic-types.html
	@see https://haxe.org/manual/types-nullability.html
**/
@:coreType @:notNull @:runtimeValue abstract Float {}

/**
	The standard `Int` type. Its precision depends on the platform.

	On static targets, `null` cannot be assigned to `Int`. If this is necessary,
	`Null<Int>` can be used instead.

	`Std.int` converts a `Float` to an `Int`, rounded towards 0.
	`Std.parseInt` converts a `String` to an `Int`.

	@see https://haxe.org/manual/types-basic-types.html
	@see https://haxe.org/manual/std-math-integer-math.html
	@see https://haxe.org/manual/types-nullability.html
**/
@:coreType @:notNull @:runtimeValue abstract Int to Float {}

#if (java || hl || cpp)
/**
	Single-precision IEEE 32bit float (4-byte).
**/
@:coreType @:notNull @:runtimeValue abstract Single to Float from Float {}
#end

/**
	`Null<T>` is a wrapper that can be used to make the basic types `Int`,
	`Float` and `Bool` nullable on static targets.

	If null safety is enabled, only types wrapped in `Null<T>` are nullable.

	Otherwise, it has no effect on non-basic-types, but it can be useful as a way to document
	that `null` is an acceptable value for a method argument, return value or variable.

	@see https://haxe.org/manual/types-nullability.html
**/
@:forward
@:coreType
abstract Null<T> from T to T {}

/**
	The standard Boolean type, which can either be `true` or `false`.

	On static targets, `null` cannot be assigned to `Bool`. If this is necessary,
	`Null<Bool>` can be used instead.

	@see https://haxe.org/manual/types-bool.html
	@see https://haxe.org/manual/types-nullability.html
**/
@:coreType @:notNull @:runtimeValue abstract Bool {}

/**
	`Dynamic` is a special type which is compatible with all other types.

	Use of `Dynamic` should be minimized as it prevents several compiler
	checks and optimizations. See `Any` type for a safer alternative for
	representing values of any type.

	@see https://haxe.org/manual/types-dynamic.html
**/
@:coreType @:runtimeValue abstract Dynamic<T> {}

/**
	An `Iterator` is a structure that permits iteration over elements of type `T`.

	Any class with matching `hasNext()` and `next()` fields is considered an `Iterator`
	and can then be used e.g. in `for`-loops. This makes it easy to implement
	custom iterators.

	@see https://haxe.org/manual/lf-iterators.html
**/
typedef Iterator<T> = {
	/**
		Returns `false` if the iteration is complete, `true` otherwise.

		Usually iteration is considered to be complete if all elements of the
		underlying data structure were handled through calls to `next()`. However,
		in custom iterators any logic may be used to determine the completion
		state.
	**/
	function hasNext():Bool;

	/**
		Returns the current item of the `Iterator` and advances to the next one.

		This method is not required to check `hasNext()` first. A call to this
		method while `hasNext()` is `false` yields unspecified behavior.

		On the other hand, iterators should not require a call to `hasNext()`
		before the first call to `next()` if an element is available.
	**/
	function next():T;
}

/**
	An `Iterable` is a data structure which has an `iterator()` method.
	See `Lambda` for generic functions on iterable structures.

	@see https://haxe.org/manual/lf-iterators.html
**/
typedef Iterable<T> = {
	function iterator():Iterator<T>;
}

/**
	A `KeyValueIterator` is an `Iterator` that has a key and a value.
**/
typedef KeyValueIterator<K, V> = Iterator<{key:K, value:V}>;

/**
	A `KeyValueIterable` is a data structure which has a `keyValueIterator()`
	method to iterate over key-value-pairs.
**/
typedef KeyValueIterable<K, V> = {
	function keyValueIterator():KeyValueIterator<K, V>;
}

/**
	`ArrayAccess` is used to indicate a class that can be accessed using brackets.
	The type parameter represents the type of the elements stored.

	This interface should be used for externs only. Haxe does not support custom
	array access on classes. However, array access can be implemented for
	abstract types.

	@see https://haxe.org/manual/types-abstract-array-access.html
**/
extern interface ArrayAccess<T> {}
