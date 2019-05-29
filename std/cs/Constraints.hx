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

package cs;

/**
	The type argument must be a value type. Any value type except Nullable_1<T>
	can be specified.

	It is intended to be used as a native cs type parameter constraint, when
	using `@:nativeGen`. This constraint won't have any effect on Haxe code.
	If used as a real type, the underlying type will be `Dynamic`.
**/
@:coreType abstract CsStruct from Dynamic {}

/**
	The type argument must be a reference type. This constraint applies also to
	any class, interface, delegate, or array type.

	It is intended to be used as a native cs type parameter constraint, when
	using `@:nativeGen`. This constraint won't have any effect on Haxe code.
	If used as a real type, the underlying type will be `Dynamic`.
**/
@:coreType abstract CsClass from Dynamic {}

#if (cs_ver >= "7.3")
/**
	The type argument must not be a reference type and must not contain any
	reference type members at any level of nesting.

	It is intended to be used as a native cs type parameter constraint, when
	using `@:nativeGen`. This constraint won't have any effect on Haxe code.
	If used as a real type, the underlying type will be `Dynamic`.
**/
@:coreType abstract CsUnmanaged from Dynamic {}
#end
