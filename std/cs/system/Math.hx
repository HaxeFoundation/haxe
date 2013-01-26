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
package cs.system;

@:native("System.Math") @:nativeGen extern class Math
{
	public static var PI(default, null) : Float;

	public static function Abs(v:Float):Float;
	public static function Min(a:Float,b:Float):Float;
	public static function Max(a:Float,b:Float):Float;
	public static function Sin(v:Float):Float;
	public static function Cos(v:Float):Float;
	public static function Atan2(y:Float,x:Float):Float;
	public static function Tan(v:Float):Float;
	public static function Exp(v:Float):Float;
	public static function Log(v:Float):Float;
	public static function Sqrt(v:Float):Float;
	public static function Round(v:Float):Float;
	public static function Floor(v:Float):Float;
	public static function Ceiling(v:Float):Float;
	public static function Atan(v:Float):Float;
	public static function Asin(v:Float):Float;
	public static function Acos(v:Float):Float;
	public static function Pow(v:Float,exp:Float):Float;
}