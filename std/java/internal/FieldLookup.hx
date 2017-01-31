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
package java.internal;

import java.lang.System;

@:native('haxe.lang.FieldLookup')
@:keep
@:static private class FieldLookup
{

	@:functionCode('
		return s.hashCode();
	')
	public static function hash(s:String):Int
	{
		return 0;
	}

	public static function findHash(hash:String, hashs:java.NativeArray<String>, length:Int):Int
	{
		var min = 0;
		var max = length;

		while (min < max)
		{
			var mid = Std.int((max + min) / 2); //overflow safe
			var classify = untyped hash.compareTo(hashs[mid]);
			if (classify < 0)
			{
				max = mid;
			} else if (classify > 0) {
				min = mid + 1;
			} else {
				return mid;
			}
		}
		//if not found, return a negative value of where it should be inserted
		return ~min;
	}

	static function removeString(a:java.NativeArray<String>, length:Int, pos:Int) {
		System.arraycopy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = null;
	}

	static function removeFloat(a:java.NativeArray<Float>, length:Int, pos:Int) {
		System.arraycopy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = 0;
	}

	static function removeDynamic(a:java.NativeArray<Dynamic>, length:Int, pos:Int) {
		System.arraycopy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = null;
	}

	@:extern
	static inline function __insert<T>(a:java.NativeArray<T>, length:Int, pos:Int, x:T):java.NativeArray<T>
	{
		var capacity = a.length;
		if (pos == length)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				System.arraycopy(a, 0, newarr, 0, a.length);
				a = newarr;
			}
		}
		else if (pos == 0)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				System.arraycopy(a, 0, newarr, 1, length);
				a = newarr;
			}
			else
			{
				System.arraycopy(a, 0, a, 1, length);
			}
		}
		else
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				System.arraycopy(a, 0, newarr, 0, pos);
				System.arraycopy(a, pos, newarr, pos + 1, length - pos);
				a = newarr;
			}
			else
			{
				System.arraycopy(a, pos, a, pos + 1, length - pos);
				System.arraycopy(a, 0, a, 0, pos);
			}
		}
		a[pos] = x;
		return a;
	}

	static function insertString(a:java.NativeArray<String>, length:Int, pos:Int, x:String):java.NativeArray<String> return __insert(a, length, pos, x);
	static function insertFloat(a:java.NativeArray<Float>, length:Int, pos:Int, x:Float):java.NativeArray<Float> return __insert(a, length, pos, x);
	static function insertDynamic(a:java.NativeArray<Dynamic>, length:Int, pos:Int, x:Dynamic):java.NativeArray<Dynamic> return __insert(a, length, pos, x);
}
