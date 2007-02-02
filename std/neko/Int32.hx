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
package neko;

class Int32 {

	public static var ofInt : Int -> Int32 = neko.Lib.load("std","int32_new",1);
	public static var toInt : Int32 -> Int = neko.Lib.load("std","int32_to_int",1);
	public static var toFloat : Int32 -> Float = neko.Lib.load("std","int32_to_float",1);
	public static var address : Dynamic -> Int32 = neko.Lib.load("std","int32_address",1);
	public static var add : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_add",2);
	public static var sub : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_sub",2);
	public static var mul : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_mul",2);
	public static var div : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_div",2);
	public static var mod : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_mod",2);
	public static var shl : Int32 -> Int -> Int32 = neko.Lib.load("std","int32_shl",2);
	public static var shr : Int32 -> Int -> Int32 = neko.Lib.load("std","int32_shr",2);
	public static var ushr : Int32 -> Int -> Int32 = neko.Lib.load("std","int32_ushr",2);
	public static var and : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_and",2);
	public static var or : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_or",2);
	public static var xor : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_xor",2);
	public static var neg : Int32 -> Int32 = neko.Lib.load("std","int32_neg",1);
	public static var complement : Int32 -> Int32 = neko.Lib.load("std","int32_complement",1);
	public static var compare : Int32 -> Int32 -> Int32 = neko.Lib.load("std","int32_compare",2);

}
