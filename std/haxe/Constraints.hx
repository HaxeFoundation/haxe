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

/**
	This type unifies with any function type.
	
	It is intended to be used as a type parameter constraint. If used as a real
	type, the underlying type will be `Dynamic`.
**/
abstract Function(Dynamic) { }

/**
	This type unifies with an enum instance if all constructors of the enum
	require no arguments.
	
	It is intended to be used as a type parameter constraint. If used as a real
	type, the underlying type will be `Dynamic`.
**/
abstract FlatEnum(Dynamic) { }

/**
	This type is compatible with both its type parameters.
	
	It is intended to be used as a type parameter constraint. If used as a real
	type, the underlying type will be `Dynamic`.
**/
abstract Or<L,R>(Dynamic) from L to L from R to R { }

/**
	The types allowed as key to `haxe.ds.ObjectMap`.
**/
extern typedef ObjectMapKey = Or<Class<Dynamic>, {}>;
