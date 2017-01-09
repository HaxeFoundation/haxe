/*
 * Copyright (C)2005-2017 Haxe Foundation
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

package lua;

/**
  Externs for the "bit" class that is required for Haxe lua
**/
@:native("_hx_bit")
extern class Bit {
	public static function bnot(x:Float) : Int;
	public static function band(a:Float, b:Float) : Int;
	public static function bor(a:Float, b:Float) : Int;
	public static function bxor(a:Float, b:Float) : Int;
	public static function lshift(x:Float, places:Int) : Int;
	public static function rshift(x:Float, places:Int) : Int;
	public static function arshift(x:Float, places:Int) : Int;
	public static function mod(numerator:Float, denominator:Float) : Int;
	public static function __init__() : Void {
		untyped _hx_bit = __define_feature__("use._bitop",_hx_bit);
	}
}
