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

package cpp;

@:noPackageRestrict
extern class NativeMath {
	#if (cpp && !cppia)
	@:native("_hx_idiv")
	static function idiv(num:Int, denom:Int):Int;
	@:native("_hx_imod")
	static function imod(num:Int, denom:Int):Int;
	@:native("_hx_cast_int")
	static function castInt(f:Float):Int;
	@:native("_hx_fast_floor")
	static function fastInt(f:Float):Int;
	#else
	static inline function imod(num:Int, denom:Int):Int
		return num % denom;

	static inline function idiv(num:Int, denom:Int):Int
		return Std.int(num / denom);

	static inline function castInt(f:Float):Int
		return Std.int(f);

	static inline function fastInt(f:Float):Int
		return Std.int(f);
	#end
}
