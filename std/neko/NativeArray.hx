/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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
package neko;

class NativeArray<T> implements ArrayAccess<T> {

	public static inline function alloc<T>( length : Int ) : NativeArray<T> {
		return untyped __dollar__amake(length);
	}

	public static inline function blit<T>( dst : NativeArray<T>, dstPos : Int, src : NativeArray<T>, srcPos : Int, length : Int ) {
		return untyped __dollar__ablit(dst,dstPos,src,srcPos,length);
	}

	public static inline function ofArrayCopy<T>( a : Array<T> ) : NativeArray<T> {
		return untyped a.__neko();
	}

	public static inline function ofArrayRef<T>( a : Array<T> ) : NativeArray<T> {
		return untyped a.__a;
	}

	public static inline function toArray<T>( a : NativeArray<T> ) : Array<T> {
		return untyped Array.new1(a,__dollar__asize(a));
	}

	public static inline function length( a : NativeArray<Dynamic> ) : Int {
		return untyped __dollar__asize(a);
	}

}