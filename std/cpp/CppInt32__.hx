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
package cpp;

typedef Int32 = CppInt32__;

extern class CppInt32__ {
	public static  function make( a : Int, b : Int ) : Int32;
	public static  function ofInt( x : Int ) : Int32;
	public static  function toInt( x : Int32 ) : Int;
	public static  function add( a : Int32, b : Int32 ) : Int32;
	public static  function sub( a : Int32, b : Int32 ) : Int32;
	public static  function mul( a : Int32, b : Int32 ) : Int32;
	public static  function div( a : Int32, b : Int32 ) : Int32;
	public static  function mod( a : Int32, b : Int32 ) : Int32;
	public static  function shl( a : Int32, b : Int ) : Int32;
	public static  function shr( a : Int32, b : Int ) : Int32;
	public static  function ushr( a : Int32, b : Int ) : Int32;
	public static  function and( a : Int32, b : Int32 ) : Int32;
	public static  function or( a : Int32, b : Int32 ) : Int32;
	public static  function xor( a : Int32, b : Int32 ) : Int32;
	public static  function neg( a : Int32 ) : Int32;
	public static  function complement( a : Int32 ) : Int32;
	public static  function compare( a : Int32, b : Int32 ) : Int;
}

