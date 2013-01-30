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
// standard haXe types

/**
	The standard Void type. Only [null] values can be of the type [Void].
**/
abstract Void { }

/**
	The standard Float type, this is a double-precision IEEE 64bit float.
**/
@:notNull @:runtimeValue abstract Float { }

/**
	The standard Int type. Its precision depends on the platform.
**/
@:coreType @:notNull @:runtimeValue abstract Int to Float { }

#if (flash9 || flash9doc || cs)
/**
	The unsigned Int type is only defined for Flash9. It's currently
	handled the same as a normal Int.
**/
@:coreType @:notNull @:runtimeValue abstract UInt to Int from Int { }
#end

#if (java || cs)
@:coreType @:notNull @:runtimeValue abstract Single to Float from Float {}
#end

/**
	[Null] can be useful in two cases. In order to document some methods
	that accepts or can return a [null] value, or for the Flash9 compiler and AS3
	generator to distinguish between base values that can be null and others that
	can't.
**/
typedef Null<T> = T

/**
	The standard Boolean type is represented as an enum with two choices.
**/
@:notNull @:runtimeValue abstract Bool {
}

/**
	Dynamic is an internal compiler type which has special behavior.
	See the haXe language reference for more informations.
**/
@:runtimeValue abstract Dynamic<T> {
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
	See [Lambda] for generic functions on iterable structures.
**/
typedef Iterable<T> = {
	function iterator() : Iterator<T>;
}

/**
	ArrayAccess is used to indicate a class that can be accessed using brackets.
	The type parameter represent the type of the elements stored.
**/
extern interface ArrayAccess<T> { }
