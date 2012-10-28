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
@:notNull @:runtimeValue abstract Int <= Float { }

#if (flash9 || flash9doc || cs)
/**
	The unsigned Int type is only defined for Flash9. It's currently
	handled the same as a normal Int.
**/
@:notNull @:runtimeValue abstract UInt => Int, <= Int { }
#end

#if (java || cs)
@:notNull @:runtimeValue abstract Single => Float, <= Float {}
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
	An Iterator is a structure that permits to list a given container
	values. It can be used by your own data structures. See the haXe
	documentation for more informations.
**/
typedef Iterator<T> = {
	function hasNext() : Bool;
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
