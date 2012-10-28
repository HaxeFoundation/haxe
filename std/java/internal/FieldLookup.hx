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
package java.internal;

@:native('haxe.lang.FieldLookup')
@:keep
@:static private class FieldLookup 
{
	
	@:functionBody('
		return s.hashCode();
	')
	public static function hash(s:String):Int
	{
		return 0;
	}
	
	public static function findHash(hash:String, hashs:Array<String>):Int
	{
		var min = 0;
		var max = hashs.length;
		
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
	
}