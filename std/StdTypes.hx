/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
